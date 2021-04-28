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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Unrolling = struct
  type t =
    | Not_unrolling
    | Unrolling of { remaining_depth : int }
    | Done

  let print ppf = function
    | Not_unrolling ->
      Format.pp_print_string ppf "not_unrolling"
    | Unrolling { remaining_depth } ->
      Format.fprintf ppf "@[<hov 1>(remaining_depth@ %d)@]" remaining_depth
    | Done ->
      Format.pp_print_string ppf "done"

  let equal t1 t2 =
    match t1, t2 with
    | Not_unrolling, Not_unrolling -> true
    | Unrolling { remaining_depth = rd1 },
      Unrolling { remaining_depth = rd2 } -> rd1 = rd2
    | Done, Done -> true
    | (Not_unrolling | Unrolling _ | Done), _ -> false

  (** The unrolling state one level deeper. *)
  let succ t =
    match t with
    | Not_unrolling
    | Done -> t
    | Unrolling { remaining_depth = 1 } -> Done
    | Unrolling { remaining_depth = d } -> Unrolling { remaining_depth = d - 1 }
end

type t = {
  depth : int;
  unrolling : Unrolling.t;
  is_self_reference : bool;
}

let print ppf { depth; unrolling; is_self_reference } =
  Format.fprintf ppf "%s@[<hov 1>(\
      @[<hov 1>(depth@ %d)@]@ \
      @[<hov 1>(unrolling@ %a)@]@ \
      @[<hov 1>(is_self_reference@ %b)@]\
      )@]%s"
    (Flambda_colours.rec_info ())
    depth
    Unrolling.print unrolling
    is_self_reference
    (Flambda_colours.normal ())

let equal
      { depth = depth1; unrolling = unrolling1 }
      { depth = depth2; unrolling = unrolling2 } =
  depth1 = depth2 && Unrolling.equal unrolling1 unrolling2

let create ~depth ~unrolling ~is_self_reference =
  { depth;
    unrolling;
    is_self_reference;
  }

let depth t = t.depth
let unrolling t = t.unrolling
let is_self_reference t = t.is_self_reference

let initial = create ~depth:0 ~unrolling:Not_unrolling ~is_self_reference:false

let is_initial = function
  | { depth = 0; unrolling = Not_unrolling; is_self_reference = false } -> true
  | _ -> false

let initial_self_reference =
  create ~depth:0 ~unrolling:Not_unrolling ~is_self_reference:true

let succ { depth; unrolling; is_self_reference } =
  let depth = depth + 1 in
  let unrolling = Unrolling.succ unrolling in
  { depth; unrolling; is_self_reference }
