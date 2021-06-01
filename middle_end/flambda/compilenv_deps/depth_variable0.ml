(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  module Variable : Identifiable.S

  type t = private Variable.t

  include Identifiable.S with type t := t

  (** Wrap a variable as a depth variable. The variable must have kind
      [Rec_info]. *)
  val of_var : Variable.t -> t

  val var : t -> Variable.t

  module Or_zero : sig
    type depth_variable := t

    type t = private
      | Var of depth_variable
      | Zero

    include Identifiable.S with type t := t

    val var : depth_variable -> t
    val zero : t

    val map_var : t -> f:(depth_variable -> depth_variable) -> t
  end
end

module Make(Variable : Identifiable.S) : S with module Variable = Variable =
struct
  module Variable = Variable

  type t = Variable.t

  include Identifiable.Make(struct
    type nonrec t = t

    let compare = Variable.compare
    let hash = Variable.hash
    let equal = Variable.equal

    let print ppf t =
      Format.fprintf ppf "@<0>%s%a@<0>%s"
        (Flambda_colours.depth_variable ())
        Variable.print t
        (Flambda_colours.normal ())

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let of_var t = t
  let var t = t

  module Or_zero = struct
    module T0 = struct
      type nonrec t =
        | Var of t
        | Zero

      let print ppf = function
        | Var dv -> print ppf dv
        | Zero -> Format.pp_print_string ppf "0"

      let output out t = print (Format.formatter_of_out_channel out) t

      let compare t1 t2 =
        match t1, t2 with
        | Zero, Zero -> 0
        | Zero, Var _ -> -1
        | Var _, Zero -> 1
        | Var dv1, Var dv2 -> compare dv1 dv2

      let equal t1 t2 = (compare t1 t2 = 0)

      let hash = function
        | Var t -> Hashtbl.hash (0, hash t)
        | Zero -> Hashtbl.hash 1

    end

    include T0
    include Identifiable.Make(T0)

    let var t = Var t
    let zero = Zero

    let map_var t ~f =
      match t with
      | Var dv ->
        let new_dv = f dv in
        if new_dv == dv then t else Var new_dv
      | Zero -> Zero
  end
end
