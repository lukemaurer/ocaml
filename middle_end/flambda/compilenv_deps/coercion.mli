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

val change_depth : from:Depth_expr0.t -> to_:Depth_expr0.t -> t

val id : t

val is_obviously_id : t -> bool

val inverse : t -> t

val compose : t -> then_:t -> t

val print : Format.formatter -> t -> unit

val interpret
   : t
 -> change_depth:('a -> from:Depth_expr0.t -> to_:Depth_expr0.t -> 'a)
 -> 'a
 -> 'a

val equal : t -> t -> bool

val hash : t -> int
