(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Mark Shinwell, Jane Street Europe                    *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

module Unrolling : sig
  type t =
    | Not_unrolling
    | Unrolling of { remaining_depth : int } (* >= 1 *)
    | Done

  val print : Format.formatter -> t -> unit
end

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool

val create : depth:int -> unrolling:Unrolling.t -> is_self_reference:bool -> t

val depth : t -> int

val unrolling : t -> Unrolling.t

val is_self_reference : t -> bool

val initial : t

val is_initial : t -> bool

val initial_self_reference : t

val succ : t -> t

val unroll : by_depth:int -> t -> t
