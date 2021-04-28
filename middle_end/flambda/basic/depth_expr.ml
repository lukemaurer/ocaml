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

include Depth_expr0

let print_with_cache ~cache:_ ppf t = print ppf t

let rec all_ids_for_export = function
  | Zero
  | Var _ -> Ids_for_export.empty
  | Succ t
  | Unroll_to (_, t) -> all_ids_for_export t

let rec free_names = function
  | Zero -> Name_occurrences.empty
  | Var v -> Name_occurrences.singleton_depth_variable v
  | Succ t
  | Unroll_to (_, t) -> free_names t

let rec apply_renaming t perm =
  match t with
  | Zero -> Zero
  | Var v -> Var (Renaming.apply_depth_variable perm v)
  | Succ t -> Succ (apply_renaming t perm)
  | Unroll_to (d, t) -> Unroll_to (d, apply_renaming t perm)

let rec invariant env = function
  | Zero -> ()
  | Var v -> Invariant_env.check_depth_variable_is_bound env v
  | Succ t -> invariant env t
  | Unroll_to (d, t) -> assert (d >= 0); invariant env t
