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

let all_ids_for_export { offset = _; var; unroll_depth = _ } =
  match var with
  | None -> Ids_for_export.empty
  | Some var -> Ids_for_export.singleton_depth_variable var

let import import_map { offset; var; unroll_depth } =
  let var =
    Option.map (Ids_for_export.Import_map.depth_variable import_map) var
  in
  { offset; var; unroll_depth }

let free_names { offset = _; var; unroll_depth = _ } =
  match var with
  | None -> Name_occurrences.empty
  | Some var -> Name_occurrences.singleton_depth_variable var Name_mode.normal

let apply_name_permutation { offset; var; unroll_depth } perm =
  let var = Option.map (Name_permutation.apply_depth_variable perm) var in
  { offset; var; unroll_depth }

let invariant env { offset; var; unroll_depth } =
  assert (offset >= 0);
  Option.iter (fun unroll_depth -> assert (unroll_depth >= 0)) unroll_depth;
  Option.iter (Invariant_env.check_depth_variable_is_bound env) var

