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

let rec apply_renaming t perm =
  match t with
  | Const _ -> t
  | Var dv ->
    let dv =
      Renaming.apply_variable perm (Depth_variable.var dv)
      |> Depth_variable.of_var
    in
    Var dv
  | Succ t ->
    Succ (apply_renaming t perm)
  | Unroll_to (unroll_depth, t) ->
    Unroll_to (unroll_depth, apply_renaming t perm)

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

let compute_succ ~(depth : int Or_infinity.t) ~(unroll_to : int option) =
  let depth : int Or_infinity.t =
    match depth with
    | Finite n -> Finite (n+1)
    | Infinity -> Infinity
  in
  let unroll_to =
    match unroll_to with
    | None | Some 0 -> unroll_to
    | Some n -> Some (n-1)
  in
  Const { depth; unroll_to }

let compute_unroll_to ~depth ~old_unroll_to ~new_unroll_to =
  (* Take the maximum of the two unroll depths. This allows an external
     caller to specify more unrolling than the recursive call sites do. *)
  let unroll_to =
    match old_unroll_to with
    | None ->
      Some new_unroll_to
    | Some unroll_to ->
      if unroll_to >= new_unroll_to then old_unroll_to else
        Some new_unroll_to
  in
  Const { depth; unroll_to }

let rec simplify orig_t ~find_var =
  match orig_t with
  | Const _ -> orig_t
  | Var dv ->
    begin match find_var dv with
    | Some t ->
      (* All bound names are fresh, so fine to use the same environment *)
      simplify t ~find_var
    | None ->
      orig_t
    end
  | Succ t ->
    begin match simplify t ~find_var with
    | Const { depth; unroll_to } ->
      compute_succ ~depth ~unroll_to
    | (Var _ | Succ _ | Unroll_to _) as new_t ->
      if t == new_t then orig_t else Succ new_t
    end
  | Unroll_to (unroll_depth, t) ->
    begin match simplify t ~find_var with
    | Const { depth; unroll_to } ->
      compute_unroll_to ~depth
        ~old_unroll_to:unroll_to ~new_unroll_to:unroll_depth
    | (Var _ | Succ _ | Unroll_to _) as new_t ->
      if t == new_t then orig_t else Unroll_to (unroll_depth, new_t)
    end
