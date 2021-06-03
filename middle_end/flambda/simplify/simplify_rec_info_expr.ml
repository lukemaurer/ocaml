(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module DA = Downwards_acc
module DE = Downwards_env
module T = Flambda_type

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
  Rec_info_expr.const ~depth ~unroll_to

let compute_unroll_to ~depth ~old_unroll_to ~new_unroll_to =
  (* Take the maximum of the two unroll depths. This allows an external
     caller to specify more unrolling than the recursive call sites do. *)
  (* CR lmaurer: Can also see an argument for making this take the minimum.
     What does FLambda 1 do? *)
  let unroll_to =
    match old_unroll_to with
    | None ->
      Some new_unroll_to
    | Some unroll_to ->
      if unroll_to >= new_unroll_to then old_unroll_to else
        Some new_unroll_to
  in
  Rec_info_expr.const ~depth ~unroll_to

type on_unknown =
  | Leave_unevaluated
  | Assume_value of Rec_info_expr.t

let rec simplify_rec_info_expr0 denv orig ~on_unknown : Rec_info_expr.t =
  (fun ans -> if !Clflags.dump_rawflambda then begin
    Format.eprintf "@[<hov 1>simplify_rec_info_expr@ %a@ = %a@]@.%!"
      Rec_info_expr.print orig
      Rec_info_expr.print ans
  end;
  ans) @@ ((
  match (orig : Rec_info_expr.t) with
  | Const _ -> orig
  | Var dv ->
    let ty = DE.find_variable denv (Depth_variable.var dv) in
    begin match T.prove_rec_info (DE.typing_env denv) ty with
    | Proved rec_info_expr ->
      (* All bound names are fresh, so fine to use the same environment *)
      simplify_rec_info_expr0 denv rec_info_expr ~on_unknown
    | Unknown ->
      begin match on_unknown with
      | Leave_unevaluated -> orig
      | Assume_value value -> value
      end
    | Invalid ->
      (* Shouldn't currently be possible *)
      Misc.fatal_errorf "Invalid result from [prove_rec_info] of %a" T.print ty
    end
  | Succ ri ->
    begin match simplify_rec_info_expr0 denv ri ~on_unknown with
    | Const { depth; unroll_to } ->
      compute_succ ~depth ~unroll_to
    | (Var _ | Succ _ | Unroll_to _) as new_ri ->
      if ri == new_ri then orig else Rec_info_expr.succ new_ri
    end
  | Unroll_to (unroll_depth, ri) ->
    begin match simplify_rec_info_expr0 denv ri ~on_unknown with
    | Const { depth; unroll_to } ->
      compute_unroll_to ~depth
        ~old_unroll_to:unroll_to ~new_unroll_to:unroll_depth
    | (Var _ | Succ _ | Unroll_to _) as new_ri ->
      if ri == new_ri then orig else Rec_info_expr.unroll_to unroll_depth new_ri
    end
) : Rec_info_expr.t)

let simplify_rec_info_expr dacc rec_info_expr =
  let ans =
  simplify_rec_info_expr0 (DA.denv dacc) rec_info_expr
    ~on_unknown:Leave_unevaluated
  in
  ans

module Evaluated_rec_info_expr = struct
  type t = {
    depth : int Or_infinity.t;
    unroll_to : int option;
  }

  let print ppf { depth; unroll_to } =
    Format.fprintf ppf
      "@[<hov 1>\
       @[<hov 1>(depth@ %a)@]@ \
       @[<hov 1>(unroll_to@ %a)@]@]"
      (Or_infinity.print ~f:Format.pp_print_int) depth
      (Misc.Stdlib.Option.print Format.pp_print_int) unroll_to
end

let evaluate_rec_info_expr dacc rec_info_expr =
  match
    simplify_rec_info_expr0 (DA.denv dacc) rec_info_expr
      ~on_unknown:(Assume_value Rec_info_expr.unknown)
  with
  | Const { depth; unroll_to } ->
    { Evaluated_rec_info_expr.depth; unroll_to }
  | Var _ | Succ _ | Unroll_to _ ->
    Misc.fatal_errorf "Unable to evaluate@ %a@ with@ dacc@ %a"
      Rec_info_expr.print rec_info_expr
      DA.print dacc

let depth_may_be_at_least dacc rec_info_expr bound =
  let { Evaluated_rec_info_expr.depth; _ } =
    evaluate_rec_info_expr dacc rec_info_expr
  in
  Or_infinity.compare ~f:Int.compare depth (Finite bound) >= 1

let known_unrolling_depth dacc rec_info_expr =
  let { Evaluated_rec_info_expr.unroll_to; _ } =
    evaluate_rec_info_expr dacc rec_info_expr
  in unroll_to
