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

module type S = sig
  type depth_variable

  module Unrolling_state : sig
    type t = private
      | Not_unrolling
      | Unrolling of { remaining_depth : int }
      | Do_not_unroll

    val not_unrolling : t
    val unrolling : remaining_depth:int -> t
    val do_not_unroll : t

    val print : Format.formatter -> t -> unit

    val equal : t -> t -> bool

    val hash : t -> int
  end

  type t = private
    | Const of { depth : int Or_infinity.t; unrolling : Unrolling_state.t }
    | Var of depth_variable
    | Succ of t
    | Unroll_to of int * t

  val initial : t
  val unknown : t
  val do_not_inline : t
  val const : depth:int Or_infinity.t -> unrolling:Unrolling_state.t -> t
  val var : depth_variable -> t
  val succ : t -> t
  val unroll_to : int -> t -> t

  val is_obviously_initial : t -> bool

  val print : Format.formatter -> t -> unit

  val equal : t -> t -> bool

  val hash : t -> int

  val map_depth_variables : t -> f:(depth_variable -> depth_variable) -> t
end

module Make(Depth_variable : Depth_variable0.S)
  : S with type depth_variable = Depth_variable.t
= struct
  type depth_variable = Depth_variable.t

  module Unrolling_state = struct
    type t =
      | Not_unrolling
      | Unrolling of { remaining_depth : int }
      | Do_not_unroll

    let not_unrolling = Not_unrolling
    let unrolling ~remaining_depth = Unrolling { remaining_depth; }
    let do_not_unroll = Do_not_unroll

    let print ppf = function
      | Not_unrolling ->
        Format.pp_print_string ppf "Not_unrolling"
      | Unrolling { remaining_depth } ->
        Format.fprintf ppf
          "@[<hov 1>(Unrolling@ \
           @[<hov 1>(remaining_depth@ %d)@])@]"
          remaining_depth
      | Do_not_unroll ->
        Format.pp_print_string ppf "Do_not_unroll"

    let equal t1 t2 =
      match t1, t2 with
      | Not_unrolling, Not_unrolling -> true
      | Unrolling { remaining_depth = remaining_depth1 },
        Unrolling { remaining_depth = remaining_depth2 } ->
        remaining_depth1 = remaining_depth2
      | Do_not_unroll, Do_not_unroll -> true
      | (Not_unrolling | Unrolling _ | Do_not_unroll), _ -> false

    let hash = function
      | Not_unrolling ->
        Hashtbl.hash 0
      | Unrolling { remaining_depth } ->
        Hashtbl.hash (1, remaining_depth)
      | Do_not_unroll ->
        Hashtbl.hash 2
  end

  type t =
    | Const of { depth : int Or_infinity.t; unrolling : Unrolling_state.t }
    | Var of depth_variable
    | Succ of t
    | Unroll_to of int * t

  let initial = Const { depth = Finite 0; unrolling = Not_unrolling }
  let unknown = Const { depth = Infinity; unrolling = Not_unrolling }
  let do_not_inline = Const { depth = Infinity; unrolling = Do_not_unroll }
  let const ~depth ~unrolling = Const { depth; unrolling }
  let var dv = Var dv
  let succ t = Succ t
  let unroll_to unroll_depth t = Unroll_to (unroll_depth, t)

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
      Format.fprintf ppf
        "@<0>%s@[<hov 1>(\
         @[<hov 1>(depth@ %a)@]@ \
         @[<hov 1>@<0>%s%a@<0>%s@]\
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

  let rec equal t1 t2 =
    t1 == t2
    ||
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

  let rec hash = function
    | Const { depth; unrolling } ->
      Hashtbl.hash (0,
                    Or_infinity.hash ~f:Hashtbl.hash depth,
                    Unrolling_state.hash unrolling)
    | Var dv ->
      Hashtbl.hash (1, Depth_variable.hash dv)
    | Succ t ->
      Hashtbl.hash (2, hash t)
    | Unroll_to (unroll_depth, t) ->
      Hashtbl.hash (3, unroll_depth, hash t)

  let rec map_depth_variables t ~f =
    match t with
    | Const _ -> t
    | Var dv -> Var (f dv)
    | Succ t -> Succ (map_depth_variables t ~f)
    | Unroll_to (depth, t) -> Unroll_to (depth, map_depth_variables t ~f)
end
