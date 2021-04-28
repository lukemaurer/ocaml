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
  | Inverse of t
  | Compose of {
      first : t;
      second : t
    }
  | Change_depth of {
      from : Depth_expr0.t;
      to_ : Depth_expr0.t;
    }

let id = Id

let change_depth ~from ~to_ = Change_depth { from; to_ }

let is_obviously_id = function
  | Id -> true
  | _ -> false

let inverse = function
  (* As elsewhere in this file, don't recurse but otherwise put in a best
     effort *)
  | Id -> Id
  | Inverse t -> t
  | Change_depth { from; to_ } ->
    Change_depth { from = to_; to_ = from }
  | t -> Inverse t

let rec print ppf = function
  | Id ->
    Format.fprintf ppf "@<0>%sid@<0>%s"
      (Flambda_colours.elide ())
      (Flambda_colours.normal ())
  | Inverse t ->
    Format.fprintf ppf "@[<hov 1>(%a)^-1)@]" print t
  | Compose { first; second } ->
    Format.fprintf ppf "[@<hov 1>(%a@ >>@ %a)@]" print first print second
  | Change_depth { from; to_; } ->
    Format.fprintf ppf "@<0>%s@[<hov 1>(depth@ %a ->@ %a)@]@<0>%s"
      (Flambda_colours.coercion ())
      Depth_expr0.print from
      Depth_expr0.print to_
      (Flambda_colours.normal ())

let compose t1 ~then_:t2 =
  (* It's tempting to check for composability right now, but because of depth
     variables, we can't in general know whether they're composable without
     a context.  *)
  match t1, t2 with
  | Id, _ -> t2
  | _, Id -> t1
  | _, _ -> Compose { first = t1; second = t2 }

let rec interpret t ~change_depth a =
  match t with
  | Id -> a
  | Inverse t ->
    interpret_inverse t ~change_depth a
  | Compose { first; second } ->
    interpret first ~change_depth a
    |> interpret second ~change_depth
  | Change_depth { from; to_ } ->
    change_depth a ~from ~to_
and interpret_inverse t ~change_depth a =
  match t with
  | Id -> a
  | Inverse t ->
    interpret t ~change_depth a
  | Compose { first; second } ->
    interpret_inverse second ~change_depth a
    |> interpret_inverse first ~change_depth
  | Change_depth { from; to_ } ->
    change_depth a ~from:to_ ~to_:from

let rec equal t1 t2 =
  (* This really only needs to make sense for hashing. *)
  match t1, t2 with
  | Id, Id -> true
  | Inverse t1, Inverse t2 -> equal t1 t2
  | Compose { first = first1; second = second1 },
    Compose { first = first2; second = second2 } ->
    equal first1 first2 && equal second1 second2
  | Change_depth { from = from1; to_ = to1 },
    Change_depth { from = from2; to_ = to2 } ->
    Depth_expr0.equal from1 from2 && Depth_expr0.equal to1 to2
  | (Id | Inverse _ | Compose _ | Change_depth _), _ -> false

let rec hash =
  let id_hash = Hashtbl.hash Id in
  function
  | Id -> id_hash
  | Inverse t -> Hashtbl.hash (1, hash t)
  | Compose { first; second } -> Hashtbl.hash (2, hash first, hash second)
  | Change_depth { from; to_ } -> Hashtbl.hash (3, Depth_expr0.hash from, Depth_expr0.hash to_)
