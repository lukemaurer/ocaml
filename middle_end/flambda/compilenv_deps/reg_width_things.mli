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

(** The underlying implementation for [Variable], [Symbol], [Name],
    [Reg_width_const] and [Simple]. *)

[@@@ocaml.warning "+a-30-40-41-42"]

module Const : sig
  type t = private Table_by_int_id.Id.t
  type exported

  include Identifiable.S with type t := t

  val const_true : t
  val const_false : t

  val untagged_const_true : t
  val untagged_const_false : t

  val untagged_const_zero : t

  val untagged_const_int : Targetint.OCaml.t -> t

  val const_zero : t
  val const_one : t
  val const_unit : t

  val const_int : Targetint.OCaml.t -> t

  (** [naked_immediate] is similar to [naked_nativeint], but represents
      integers of width [n - 1] bits, where [n] is the native machine
      width. (By contrast, [naked_nativeint] represents integers of
      width [n] bits.) *)
  val naked_immediate : Target_imm.t -> t
  val tagged_immediate : Target_imm.t -> t
  val naked_float : Numbers.Float_by_bit_pattern.t -> t
  val naked_int32 : Int32.t -> t
  val naked_int64 : Int64.t -> t
  val naked_nativeint : Targetint.t -> t

  module Descr : sig
    type t = private
      | Naked_immediate of Target_imm.t
      | Tagged_immediate of Target_imm.t
      | Naked_float of Numbers.Float_by_bit_pattern.t
      | Naked_int32 of Int32.t
      | Naked_int64 of Int64.t
      | Naked_nativeint of Targetint.t

    include Identifiable.S with type t := t
  end

  val descr : t -> Descr.t

  val export : t -> exported

  val import : exported -> t

  val map_compilation_unit :
    (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported
end

module Variable : sig
  type t = private Table_by_int_id.Id.t
  type exported

  include Identifiable.S with type t := t

  val create : ?user_visible:unit -> string -> t

  val compilation_unit : t -> Compilation_unit.t

  val name : t -> string

  val name_stamp : t -> int

  val user_visible : t -> bool

  val export : t -> exported

  val import : exported -> t

  val map_compilation_unit :
    (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported
end

module Depth_variable : sig
  type t = private Variable.t

  include Identifiable.S with type t := t

  val of_var : Variable.t -> t

  val var : t -> Variable.t
end

module Symbol : sig
  type t = private Table_by_int_id.Id.t
  type exported

  include Identifiable.S with type t := t

  val create : Compilation_unit.t -> Linkage_name.t -> t

  (** Create the symbol without prefixing with the compilation unit. Used for
      predefined exceptions *)
  val unsafe_create : Compilation_unit.t -> Linkage_name.t -> t

  val compilation_unit : t -> Compilation_unit.t

  val linkage_name : t -> Linkage_name.t

  val export : t -> exported

  val import : exported -> t

  val map_compilation_unit :
    (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported
end

module Name : sig
  type t = private Table_by_int_id.Id.t

  include Identifiable.S with type t := t

  val var : Variable.t -> t

  val symbol : Symbol.t -> t

  val pattern_match
     : t
    -> var:(Variable.t -> 'a)
    -> symbol:(Symbol.t -> 'a)
    -> 'a
end

module Rec_info_expr : sig
  module Unrolling_state : sig
    (** The current state of unrolling. Can be set by an [unroll_to] expression.
        *)
    type t = private
      | Not_unrolling
        (** Unrolling has not begun. *)
      | Unrolling of { remaining_depth : int }
        (** Unrolling has begun and will continue until [remaining_depth] is
            zero. A subsequent [unroll_to] expression may increase the
            remaining depth. *)
      | Do_not_unroll
        (** No unrolling may occur. [unroll_to] has no effect. *)

    val not_unrolling : t
    val unrolling : remaining_depth:int -> t
    val do_not_unroll : t

    val print : Format.formatter -> t -> unit

    val equal : t -> t -> bool
  end

  (** An expression for the state of recursive inlining at a given occurrence.
      Forms the right-hand side of a [Let_expr] binding for a depth variable. *)
  type t = private
    | Const of { depth : int Or_infinity.t; unrolling : Unrolling_state.t }
    | Var of Depth_variable.t
    | Succ of t
      (** The next depth. If we inline an occurrence with depth [d], then in the
          inlined body, recursive references will have depth [succ d]. *)
    | Unroll_to of int * t
      (** Indicate the depth to which unrolling should proceed. The unroll depth
          is decremented by [Succ] until it reaches zero, at which
          point all unrolling should stop. *)

  val initial : t
  val unknown : t
  val do_not_inline : t
  val const : depth:int Or_infinity.t -> unrolling:Unrolling_state.t -> t
  val var : Depth_variable.t -> t
  val succ : t -> t
  val unroll_to : int -> t -> t

  val is_obviously_initial : t -> bool

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool
end

module Coercion : sig
  type t = private
    | Id
    | Change_depth of {
        from : Rec_info_expr.t;
        to_ : Rec_info_expr.t;
      }

  val change_depth
    : from:Rec_info_expr.t
    -> to_:Rec_info_expr.t
    -> t

  val id : t

  (* CR lmaurer: This should be renamed to [is_obviously_id] since we can't
    guarantee in [Change_depth { from; to_ }] that [from] and [to_] are
    distinct (in any context) *)
  val is_id : t -> bool

  val inverse : t -> t

  val compose : t -> then_:t -> t option

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int

  val map_depth_variables : t -> f:(Depth_variable.t -> Depth_variable.t) -> t
end

module Simple : sig
  type t = private Table_by_int_id.Id.t
  type exported

  include Identifiable.S with type t := t

  val name : Name.t -> t

  val var : Variable.t -> t

  val vars : Variable.t list -> t list

  val symbol : Symbol.t -> t

  val const : Const.t -> t

  val coercion : t -> Coercion.t

  val with_coercion : t -> Coercion.t -> t

  (* This does not consult the grand table of [Simple]s. *)
  val has_coercion : t -> bool

  (* CR lmaurer: Should make [name] and [const] take a [coercion] argument to
     be sure we're not dropping coercions by accident. *)
  val pattern_match
     : t
    -> name:(Name.t -> coercion:Coercion.t -> 'a)
    -> const:(Const.t -> 'a)
    -> 'a

  (* [same s1 s2] returns true iff they represent the same name or const
     i.e. [same s (with_coercion s coercion)] returns true *)
  val same : t -> t -> bool

  val export : t -> exported

  val import : exported -> t

  val map_compilation_unit :
    (Compilation_unit.t -> Compilation_unit.t) -> exported -> exported
end

val initialise : unit -> unit
