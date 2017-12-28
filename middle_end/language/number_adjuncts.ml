(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module K = Flambda_kind
module T = Flambda_type

module Float_by_bit_pattern = Numbers.Float_by_bit_pattern
module Int32 = Numbers.Int32
module Int64 = Numbers.Int64

module type Num_common = sig
  include Identifiable.S

  module Pair : sig
    type nonrec t = t * t

    include Identifiable.S with type t := t
  end

  val cross_product : Set.t -> Set.t -> Pair.Set.t

  val zero : t
  val one : t
  val minus_one : t

  val add : t -> t -> t
  val sub : t -> t -> t
  val mul : t -> t -> t
  val div : t -> t -> t option
  val mod_ : t -> t -> t option

  val to_const : t -> Simple.Const.t

  val to_tagged_immediate : t -> Immediate.t
  val to_naked_float : t -> Numbers.Float_by_bit_pattern.t
  val to_naked_int32 : t -> Numbers.Int32.t
  val to_naked_int64 : t -> Numbers.Int64.t
  val to_naked_nativeint : t -> Targetint.t
end

module type Number_kind_common = sig
  module Num : Identifiable.S

  val kind : Flambda_kind.Standard_int_or_float.t

  val unboxed_prover
     : (Flambda_type.t -> Num.Set.t Flambda_type.proof)
       Flambda_type.type_accessor

  val this_unboxed : Num.t -> Flambda_type.t
  val these_unboxed : Num.Set.t -> Flambda_type.t
end

module type Number_kind = sig
  module Num : Num_common
  include Number_kind_common with module Num := Num
end

module type Int_number_kind = sig
  module Num : sig
    include Num_common

    val and_ : t -> t -> t
    val or_ : t -> t -> t
    val xor : t -> t -> t
    val shift_left : t -> Immediate.t -> t
    val shift_right : t -> Immediate.t -> t
    val shift_right_logical : t -> Immediate.t -> t
    val swap_byte_endianness : t -> t
    val neg : t -> t
  end

  include Number_kind_common with module Num := Num

  val standard_int_kind : Flambda_kind.Standard_int.t
end

module type Boxable = sig
  module Num : Identifiable.S

  val boxed_prover
     : (Flambda_type.t
         -> Num.Set.t Flambda_type.ty_naked_number Flambda_type.proof)
       Flambda_type.type_accessor

  val this_boxed : Num.t -> Flambda_type.t
  val these_boxed : Num.Set.t -> Flambda_type.t

  val box : Flambda_type.t -> Flambda_type.t
end

module type Boxable_number_kind = sig
  include Number_kind
  include Boxable with module Num := Num
end

module type Boxable_int_number_kind = sig
  include Int_number_kind
  include Boxable with module Num := Num
end

let with_shift shift if_undefined f =
  match Targetint.OCaml.to_int_option (Immediate.to_targetint shift) with
  | None ->
    (* As per a similar case in [Simplify_binary_primitive], we are here
       assigning a semantics to an operation which has undefined
       semantics. *)
    if_undefined
  | Some shift -> f shift

module For_tagged_immediates : Int_number_kind = struct
  module Num = struct
    include Immediate

    let div t1 t2 =
      if Immediate.equal t2 Immediate.zero then None
      else Some (div t1 t2)

    let mod_ t1 t2 =
      if Immediate.equal t2 Immediate.zero then None
      else Some (mod_ t1 t2)

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift)

    let shift_right t shift =
      with_shift shift zero (fun shift -> shift_right t shift)

    let shift_right_logical t shift =
      with_shift shift zero (fun shift -> shift_right_logical t shift)

    let swap_byte_endianness t =
      Immediate.map ~f:(fun i ->
          Targetint.OCaml.get_least_significant_16_bits_then_byte_swap i)
        t

    let to_const t = Simple.Const.Tagged_immediate t

    let to_tagged_immediate t = t

    (* It seems as if the various [float_of_int] functions never raise
       an exception even in the case of NaN or infinity. *)
    (* CR mshinwell: We should be sure this semantics is reasonable. *)
    let to_naked_float t =
      Float_by_bit_pattern.create (Targetint.OCaml.to_float (
        Immediate.to_targetint t))

    let to_naked_int32 t = Targetint.OCaml.to_int32 (Immediate.to_targetint t)
    let to_naked_int64 t = Targetint.OCaml.to_int64 (Immediate.to_targetint t)
    let to_naked_nativeint t =
      Targetint.OCaml.to_targetint (Immediate.to_targetint t)
  end

  let kind : K.Standard_int_or_float.t = Tagged_immediate
  let standard_int_kind : K.Standard_int.t = Tagged_immediate

  let unboxed_prover = T.prove_tagged_immediate
  let this_unboxed = T.this_tagged_immediate
  let these_unboxed = T.these_tagged_immediates
end

module For_floats : Boxable_number_kind = struct
  module Num = struct
    include Float_by_bit_pattern

    let add = IEEE_semantics.add
    let sub = IEEE_semantics.sub
    let mul = IEEE_semantics.mul
    let div t1 t2 = Some (IEEE_semantics.div t1 t2)
    let mod_ t1 t2 = Some (IEEE_semantics.mod_ t1 t2)

    let to_const t = Simple.Const.Naked_float t

    (* CR mshinwell: We need to validate that the backend compiles
       the [Int_of_float] primitive in the same way as
       [Targetint.of_float].  Ditto for [Float_of_int].  (For the record,
       [Pervasives.int_of_float] and [Nativeint.of_float] on [nan] produce
       wildly different results). *)
    let to_tagged_immediate t =
      Immediate.int (Targetint.OCaml.of_float (to_float t))

    let to_naked_float t = t
    let to_naked_int32 t = Int32.of_float (to_float t)
    let to_naked_int64 t = Int64.of_float (to_float t)
    let to_naked_nativeint t = Targetint.of_float (to_float t)
  end

  let kind : K.Standard_int_or_float.t = Naked_float

  let unboxed_prover = T.prove_naked_float
  let this_unboxed = T.this_naked_float
  let these_unboxed = T.these_naked_floats

  let boxed_prover = T.prove_boxed_float
  let this_boxed = T.this_boxed_float
  let these_boxed = T.these_boxed_floats

  let box = T.box_float
end

module For_int32s : Boxable_int_number_kind = struct
  module Num = struct
    include Int32

    let xor = logxor
    let or_ = logor
    let and_ = logand

    let div t1 t2 =
      if equal t2 zero then None
      else Some (div t1 t2)

    let mod_ t1 t2 =
      if equal t2 zero then None
      else Some (rem t1 t2)

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift)

    let shift_right t shift =
      with_shift shift zero (fun shift -> shift_right t shift)

    let shift_right_logical t shift =
      with_shift shift zero (fun shift -> shift_right_logical t shift)

    let to_const t = Simple.Const.Naked_int32 t

    let to_tagged_immediate t = Immediate.int (Targetint.OCaml.of_int32 t)
    let to_naked_float t = Float_by_bit_pattern.create (Int32.to_float t)
    let to_naked_int32 t = t
    let to_naked_int64 t = Int64.of_int32 t
    let to_naked_nativeint t = Targetint.of_int32 t
  end

  let kind : K.Standard_int_or_float.t = Naked_int32
  let standard_int_kind : K.Standard_int.t = Naked_int32

  let unboxed_prover = T.prove_naked_int32
  let this_unboxed = T.this_naked_int32
  let these_unboxed = T.these_naked_int32s

  let boxed_prover = T.prove_boxed_int32
  let this_boxed = T.this_boxed_int32
  let these_boxed = T.these_boxed_int32s

  let box = T.box_int32
end

module For_int64s : Boxable_int_number_kind = struct
  module Num = struct
    include Int64

    let xor = logxor
    let or_ = logor
    let and_ = logand

    let div t1 t2 =
      if equal t2 zero then None
      else Some (div t1 t2)

    let mod_ t1 t2 =
      if equal t2 zero then None
      else Some (rem t1 t2)

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift)

    let shift_right t shift =
      with_shift shift zero (fun shift -> shift_right t shift)

    let shift_right_logical t shift =
      with_shift shift zero (fun shift -> shift_right_logical t shift)

    let to_const t = Simple.Const.Naked_int64 t

    let to_tagged_immediate t = Immediate.int (Targetint.OCaml.of_int64 t)
    let to_naked_float t = Float_by_bit_pattern.create (Int64.to_float t)
    let to_naked_int32 t = Int64.to_int32 t
    let to_naked_int64 t = t
    let to_naked_nativeint t = Targetint.of_int64 t
  end

  let kind : K.Standard_int_or_float.t = Naked_int64
  let standard_int_kind : K.Standard_int.t = Naked_int64

  let unboxed_prover = T.prove_naked_int64
  let this_unboxed = T.this_naked_int64
  let these_unboxed = T.these_naked_int64s

  let boxed_prover = T.prove_boxed_int64
  let this_boxed = T.this_boxed_int64
  let these_boxed = T.these_boxed_int64s

  let box = T.box_int64
end

module For_nativeints : Boxable_int_number_kind = struct
  module Num = struct
    include Targetint

    let xor = logxor
    let or_ = logor
    let and_ = logand

    let div t1 t2 =
      if equal t2 zero then None
      else Some (div t1 t2)

    let mod_ t1 t2 =
      if equal t2 zero then None
      else Some (rem t1 t2)

    let shift_left t shift =
      with_shift shift zero (fun shift -> shift_left t shift)

    let shift_right t shift =
      with_shift shift zero (fun shift -> shift_right t shift)

    let shift_right_logical t shift =
      with_shift shift zero (fun shift -> shift_right_logical t shift)

    let to_const t = Simple.Const.Naked_nativeint t

    let to_tagged_immediate t = Immediate.int (Targetint.OCaml.of_targetint t)
    let to_naked_float t = Float_by_bit_pattern.create (Targetint.to_float t)
    let to_naked_int32 t = Targetint.to_int32 t
    let to_naked_int64 t = Targetint.to_int64 t
    let to_naked_nativeint t = t
  end

  let kind : K.Standard_int_or_float.t = Naked_nativeint
  let standard_int_kind : K.Standard_int.t = Naked_nativeint

  let unboxed_prover = T.prove_naked_nativeint
  let this_unboxed = T.this_naked_nativeint
  let these_unboxed = T.these_naked_nativeints

  let boxed_prover = T.prove_boxed_nativeint
  let this_boxed = T.this_boxed_nativeint
  let these_boxed = T.these_boxed_nativeints

  let box = T.box_nativeint
end