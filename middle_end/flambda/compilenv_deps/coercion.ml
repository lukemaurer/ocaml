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

type t =
  | Id
  | Change_depth of {
      from : Depth_expr.t;
      to_ : Depth_expr.t;
    }

let id = Id

let change_depth ~from ~to_ = Change_depth { from; to_ }

let is_obviously_id = function
  | Id -> true
  | Change_depth _ -> false

let inverse = function
  | Id -> Id
  | Change_depth { from; to_ } ->
    Change_depth { from = to_; to_ = from }

let print ppf = function
  | Id ->
    Format.fprintf ppf "@<0>%sid@<0>%s"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  | Change_depth { from; to_; } ->
    Format.fprintf ppf "@<0>%s@[<hov 1>(depth@ %a ->@ %a)@]@<0>%s"
      (Flambda_colours.coercion ())
      Depth_expr.print from
      Depth_expr.print to_
      (Flambda_colours.normal ())

let compose t1 ~then_:t2 =
  match t1, t2 with
  | Id, _ -> Some t2
  | _, Id -> Some t1
  | Change_depth { from = from1; to_ = to1 },
    Change_depth { from = from2; to_ = to2 } ->
    (* CR lmaurer: Doesn't feel quite right to call equal on something that
       might have a free variable. *)
    if Depth_expr.equal to1 from2 then
      Some (change_depth ~from:from1 ~to_:to2)
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
  | Change_depth { from = from1; to_ = to1 },
    Change_depth { from = from2; to_ = to2 } ->
    Depth_expr.equal from1 from2 && Depth_expr.equal to1 to2
  | _, _ -> false

let hash = Hashtbl.hash

let apply_to_depth t depth =
  match t with
  | Id ->
    Some depth
  | Change_depth { from; to_ } ->
    if Depth_expr.equal from depth then Some to_ else None
