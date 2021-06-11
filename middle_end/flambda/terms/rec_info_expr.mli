(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

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

  val equal : t -> t -> bool

  val print : Format.formatter -> t -> unit
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

val var_or_zero : Depth_variable.Or_zero.t -> t

val is_obviously_initial : t -> bool

val equal : t -> t -> bool

include Expr_std.S with type t := t

include Contains_ids.S with type t := t
