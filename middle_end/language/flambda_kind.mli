(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2017 OCamlPro SAS                                          *)
(*   Copyright 2017 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

(** Kinds of Flambda types.  These correspond in the backend to distinctions
    between different classes of registers in which variables are held
    and/or differences in GC (non-) registration of roots.
*)

module Value_kind : sig
  type t =
    | Unknown
    | Definitely_pointer
    | Definitely_immediate

  type 'a or_bottom = Ok of 'a | Bottom

  val join : t -> t -> t
  val meet : t -> t -> t or_bottom
  val compatible : t -> if_used_at:t -> bool

  val compare : t -> t -> int

  val print : Format.formatter -> t -> unit
end

module Fabricated_kind : sig
  type t =
    | Definitely_pointer
    | Definitely_immediate
end

module Phantom_kind : sig
  type t = private
    | Value of Value_kind.t
    | Naked_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint
    | Fabricated of Fabricated_kind.t
end

type t = private
  | Value of Value_kind.t
  | Naked_immediate
  | Naked_float
  | Naked_int32
  | Naked_int64
  | Naked_nativeint
  | Fabricated of Fabricated_kind.t
    (** GC-scannable values, known to be pointers, which have been introduced
        by Flambda and are never accessible at the source language level (for
        example sets of closures). *)
  | Phantom of Phantom_kind.t
    (** The kind of entities that do not exist at runtime but which are left in
        Flambda terms for the purpose of generating debugging information. *)

type kind = t

val value : Value_kind.t -> t
val naked_immediate : unit -> t
val naked_float : unit -> t
val naked_int32 : unit -> t
val naked_int64 : unit -> t
val naked_nativeint : unit -> t

val is_value : t -> bool
val is_naked_float : t -> bool

(** The kind of the unit value. *)
val unit : unit -> t

(** [compatible t ~if_used_at] returns [true] iff a value of the kind [t] may
    be used in any context with a hole expecting a value of kind [if_used_at].
*)
val compatible : t -> if_used_at:t -> bool

include Identifiable.S with type t := t

module Standard_int : sig
  (** These kinds are known as the "standard integer kinds".  They correspond
      to the usual representations of tagged immediates, 32-bit, 64-bit and
      native integers as expected by the operations in [Flambda_primitive].
      (Boxing of the latter three kinds of integers is handled via explicit
      boxing and unboxing primitives; as such, the boxed versions are not
      known as "standard". *)
  type t =
    | Tagged_immediate
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

module Standard_int_or_float : sig
  (** The same as [Standard_int], but also permitting naked floats. *)
  type t =
    | Tagged_immediate
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end

module Boxable_number : sig
  (** These kinds are those of the numbers for which a tailored boxed
      representation exists. *)

  type t =
    | Naked_float
    | Naked_int32
    | Naked_int64
    | Naked_nativeint

  (** The kind of the _unboxed_ representation of the given [t]. *)
  val to_kind : t -> kind

  val print_lowercase : Format.formatter -> t -> unit

  include Identifiable.S with type t := t
end