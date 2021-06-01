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

module Make(Depth_variable : Depth_variable0.S) = struct
  type t =
    | Id
    | Change_depth of {
        from : Depth_variable.Or_zero.t;
        to_ : Depth_variable.Or_zero.t;
      }

  let id = Id

  let change_depth ~from ~to_ =
    if Depth_variable.Or_zero.equal from to_ then Id
    else Change_depth { from; to_ }

  let is_id = function
    | Id -> true
    | Change_depth _ -> false

  let inverse = function
    | Id -> Id
    | Change_depth { from; to_ } -> Change_depth { from = to_; to_ = from }

  let print ppf = function
    | Id ->
      Format.fprintf ppf "@<0>%sid@<0>%s"
        (Flambda_colours.elide ())
        (Flambda_colours.normal ())
    | Change_depth { from; to_; } ->
      Format.fprintf ppf "@<0>%s@[<hov 1>(depth@ %a ->@ %a)@]@<0>%s"
        (Flambda_colours.coercion ())
        Depth_variable.Or_zero.print from
        Depth_variable.Or_zero.print to_
        (Flambda_colours.normal ())

  let compose t1 ~then_:t2 =
    match t1, t2 with
    | Id, _ -> Some t2
    | _, Id -> Some t1
    | Change_depth { from = from1; to_ = to_1 },
      Change_depth { from = from2; to_ = to_2 } ->
      if Depth_variable.Or_zero.equal to_1 from2 then
        Some (change_depth ~from:from1 ~to_:to_2)
      else
        None

  let compose_exn t1 ~then_:t2 =
    match compose t1 ~then_:t2 with
    | Some t -> t
    | None ->
      Misc.fatal_errorf "Invalid composition: %a@ >>@ %a" print t1 print t2

  let equal t1 t2 =
    match t1, t2 with
    | Id, Id -> true
    | Change_depth { from = from1; to_ = to_1 },
      Change_depth { from = from2; to_ = to_2 } ->
      Depth_variable.Or_zero.equal from1 from2 &&
      Depth_variable.Or_zero.equal to_1 to_2
    | (Id | Change_depth _), _ -> false

  let hash = function
    | Id ->
      Hashtbl.hash 0
    | Change_depth { from; to_ } ->
      Hashtbl.hash
        (1, Depth_variable.Or_zero.hash from, Depth_variable.Or_zero.hash to_)

  let map_depth_variables t ~f =
    match t with
    | Id -> t
    | Change_depth { from = old_from; to_ = old_to } ->
      let new_from = Depth_variable.Or_zero.map_var ~f old_from in
      let new_to = Depth_variable.Or_zero.map_var ~f old_to in
      if new_from == old_from && new_to == old_to then t else
        change_depth ~from:new_from ~to_: new_to
end
