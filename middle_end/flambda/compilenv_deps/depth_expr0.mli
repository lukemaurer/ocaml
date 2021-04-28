(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

type t =
  | Zero
  | Var of Depth_variable.t
  | Succ of t (* increase offset by 1; decrease unroll_depth by 1 if any *)
  | Unroll_to of int * t

val print : Format.formatter -> t -> unit

val equal : t -> t -> bool (* for hashing [Simple]s *)

val hash : t -> int (* for hashing [Simple]s *)
