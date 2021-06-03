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

[@@@ocaml.warning "+a-30-40-41-42"]

include Reg_width_things.Coercion

let free_names_with_mode t mode =
  match t with
  | Id -> Name_occurrences.empty
  | Change_depth { from; to_ } ->
    let add (dv : Depth_variable.Or_zero.t) names =
      match dv with
      | Zero -> names
      | Var dv ->
        Name_occurrences.add_variable names (Depth_variable.var dv) mode
    in
    Name_occurrences.empty |> add from |> add to_

let free_names t = free_names_with_mode t Name_mode.normal
let free_names_in_types t = free_names_with_mode t Name_mode.in_types

let apply_renaming t renaming =
  map_depth_variables t ~f:(fun dv ->
      Renaming.apply_variable renaming (Depth_variable.var dv)
      |> Depth_variable.of_var)
