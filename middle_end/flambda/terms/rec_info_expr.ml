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
  | Const of { depth : int Or_infinity.t; unroll_to : int option }
  | Var of Depth_variable.t
  | Succ of t
  | Unroll_to of int * t

let initial = Const { depth = Finite 0; unroll_to = None }
let unknown = Const { depth = Infinity; unroll_to = None }
let const ~depth ~unroll_to = Const { depth; unroll_to }
let var dv = Var dv
let succ t = Succ t
let unroll_to unroll_depth t = Unroll_to (unroll_depth, t)

let var_or_zero (dv_or_zero : Depth_variable.Or_zero.t) =
  match dv_or_zero with
  | Zero -> initial
  | Var dv -> var dv

let is_obviously_initial = function
  | Const { depth = Finite 0; unroll_to = None } -> true
  | Const { depth = (Finite _ | Infinity); _ }
  | Var _ | Succ _ | Unroll_to _ -> false

let rec print ppf = function
  | Const { depth; unroll_to } ->
    Format.fprintf ppf "%s@[<hov 1>(\
        @[<hov 1>(depth@ %a)@]@ \
        @[<hov 1>(unroll_to@ %a)@]\
        )@]%s"
      (Flambda_colours.rec_info ())
      (Or_infinity.print ~f:Format.pp_print_int) depth
      (Misc.Stdlib.Option.print Numbers.Int.print) unroll_to
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
  | Const { depth = depth1; unroll_to = unroll_to1 },
    Const { depth = depth2; unroll_to = unroll_to2 } ->
    Or_infinity.equal ~f:Int.equal depth1 depth2
    && Option.equal Int.equal unroll_to1 unroll_to2
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

let all_ids_for_export _ =
  (* Depth variables don't use the integer id system *)
  Ids_for_export.empty
