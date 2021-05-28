(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2018--2021 OCamlPro SAS                                    *)
(*   Copyright 2018--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

include Depth_variable

let free_names t =
  Name_occurrences.singleton_variable (var t) Name_mode.normal

let apply_renaming t perm =
  Renaming.apply_variable perm (var t) |> of_var

let all_ids_for_export t =
  Ids_for_export.add_variable Ids_for_export.empty (var t)

let print_with_cache ~cache:_ ppf t = print ppf t

let rename t = Variable.rename (var t) |> of_var

let add_to_name_permutation t ~guaranteed_fresh perm =
  Renaming.add_fresh_variable perm (var t)
    ~guaranteed_fresh:(var guaranteed_fresh)

let name_permutation t ~guaranteed_fresh =
  add_to_name_permutation t ~guaranteed_fresh Renaming.empty

let singleton_occurrence_in_terms t =
  Name_occurrences.singleton_variable (var t) Name_mode.normal

let add_occurrence_in_terms t occs =
  Name_occurrences.add_variable occs (var t) Name_mode.normal
