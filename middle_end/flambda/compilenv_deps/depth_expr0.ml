
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
  | Succ of t
  | Unroll_to of int * t

let rec print ppf = function
  | Zero ->
    Format.pp_print_string ppf "0"
  | Var v ->
    Depth_variable.print ppf v
  | Succ t ->
    let rec find_end t exp =
      match t with
      | Succ t -> find_end t (exp + 1)
      | Zero | Var _ | Unroll_to _ -> t, exp
    in
    let t, exp = find_end t 1 in
    let exp = if exp >= 2 then Format.sprintf "^%d" exp else "" in
    Format.fprintf ppf "@[<hov 1>(succ%s@ %a)@]" exp print t
  | Unroll_to (depth, t) ->
    Format.fprintf ppf "@[<hov 1>(unroll_to@ %d@ %a)]"
      depth
      print t

let rec equal t1 t2 =
  match t1, t2 with
  | Zero, Zero -> true
  | Var v1, Var v2 -> Depth_variable.equal v1 v2
  | Succ t1, Succ t2 -> equal t1 t2
  | Unroll_to (depth1, t1), Unroll_to (depth2, t2) -> depth1 = depth2 && equal t1 t2
  | (Zero | Var _ | Succ _ | Unroll_to _), _ -> false

let rec hash =
  let zero_hash = Hashtbl.hash 0 in
  function
  | Zero -> zero_hash
  | Var v -> Hashtbl.hash (1, Depth_variable.hash v)
  | Succ t -> Hashtbl.hash (2, hash t)
  | Unroll_to (d, t) -> Hashtbl.hash (3, Hashtbl.hash d, hash t)
