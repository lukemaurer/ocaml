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

open! Int_replace_polymorphic_compare

module Unrolling_state = struct
  type t =
    | Not_unrolling
    | Unrolling of { remaining_depth : int }
    | Do_not_unroll

  let not_unrolling = Not_unrolling
  let unrolling ~remaining_depth = Unrolling { remaining_depth; }
  let do_not_unroll = Do_not_unroll

  let equal t1 t2 =
    match t1, t2 with
    | Not_unrolling, Not_unrolling -> true
    | Unrolling { remaining_depth = remaining_depth1 },
      Unrolling { remaining_depth = remaining_depth2 } ->
      remaining_depth1 = remaining_depth2
    | Do_not_unroll, Do_not_unroll -> true
    | (Not_unrolling | Unrolling _ | Do_not_unroll), _ -> false

  let print ppf = function
    | Not_unrolling ->
      Format.pp_print_string ppf "Not_unrolling"
    | Unrolling { remaining_depth } ->
      Format.fprintf ppf "@[<hov 1>(Unrolling@ \
                          @[<hov 1>(remaining_depth@ %d)@])@]"
        remaining_depth
    | Do_not_unroll ->
      Format.pp_print_string ppf "Do_not_unroll"
end

type t =
  | Const of { depth : int Or_infinity.t; unrolling : Unrolling_state.t }
  | Var of Depth_variable.t
  | Succ of t
  | Unroll_to of int * t

let initial = Const { depth = Finite 0; unrolling = Not_unrolling }
let unknown = Const { depth = Infinity; unrolling = Not_unrolling }
let do_not_inline = Const { depth = Infinity; unrolling = Do_not_unroll }
let const ~depth ~unrolling = Const { depth; unrolling }
let var dv = Var dv
let succ t = Succ t
let unroll_to unroll_depth t = Unroll_to (unroll_depth, t)

let var_or_zero (dv_or_zero : Depth_variable.Or_zero.t) =
  match dv_or_zero with
  | Zero -> initial
  | Var dv -> var dv

let is_obviously_initial = function
  | Const { depth = Finite 0; unrolling = Not_unrolling } -> true
  | Const { depth = (Finite _ | Infinity);
            unrolling = (Not_unrolling | Unrolling _ | Do_not_unroll) }
  | Var _ | Succ _ | Unroll_to _ -> false

let rec print ppf = function
  | Const { depth; unrolling } ->
    let unrolling_is_default =
      match unrolling with
      | Not_unrolling -> true
      | Unrolling _ | Do_not_unroll -> false
    in
    Format.fprintf ppf "@<0>%s@[<hov 1>(\
        @[<hov 1>(depth@ %a)@]@ \
        @[<hov 1>@<0>%s(unrolling %a)@<0>%s@]\
        )@]@<0>%s"
      (Flambda_colours.rec_info ())
      (Or_infinity.print ~f:Format.pp_print_int) depth
      (if unrolling_is_default then Flambda_colours.elide () else "")
      Unrolling_state.print unrolling
      (Flambda_colours.rec_info ())
      (Flambda_colours.normal ())
  | Var dv ->
    Depth_variable.print ppf dv
  | Succ t ->
    Format.fprintf ppf "@[<hov 1>(succ@ %a)@]" print t
  | Unroll_to (unroll_depth, t) ->
    Format.fprintf ppf "@[<hov 1>(unroll_to@ %d@ %a)@]" unroll_depth print t

let print_with_cache ~cache:_ ppf t = print ppf t

let rec equal t1 t2 =
  match t1, t2 with
  | Const { depth = depth1; unrolling = unrolling1 },
    Const { depth = depth2; unrolling = unrolling2 } ->
    Or_infinity.equal ~f:Int.equal depth1 depth2
    && Unrolling_state.equal unrolling1 unrolling2
  | Var dv1, Var dv2 ->
    Depth_variable.equal dv1 dv2
  | Succ t1, Succ t2 ->
    equal t1 t2
  | Unroll_to (unroll_depth1, t1), Unroll_to (unroll_depth2, t2) ->
    unroll_depth1 = unroll_depth2 && equal t1 t2
  | (Const _ | Var _ | Succ _ | Unroll_to _), _ -> false

let rec apply_renaming orig perm =
  match orig with
  | Const _ -> orig
  | Var dv ->
    let new_dv =
      Renaming.apply_variable perm (Depth_variable.var dv)
      |> Depth_variable.of_var
    in
    if dv == new_dv then orig else Var new_dv
  | Succ t ->
    let new_t = apply_renaming t perm in
    if t == new_t then orig else Succ new_t
  | Unroll_to (unroll_depth, t) ->
    let new_t = apply_renaming t perm in
    if t == new_t then orig else Unroll_to (unroll_depth, new_t)

let rec free_names = function
  | Const _ -> Name_occurrences.empty
  | Var dv ->
    Name_occurrences.singleton_variable (Depth_variable.var dv) Name_mode.normal
  | Succ t
  | Unroll_to (_, t) -> free_names t

let invariant _ _ = ()

let rec all_ids_for_export = function
  | Const _ ->
    Ids_for_export.empty
  | Var dv ->
    Ids_for_export.add_variable Ids_for_export.empty (Depth_variable.var dv)
  | Succ t
  | Unroll_to (_, t) ->
    all_ids_for_export t
