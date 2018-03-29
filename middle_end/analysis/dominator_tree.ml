[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module C = Continuation
module Rel = Relation.Make(C)(C)

type t = {
  root_node : C.t;
  parents : C.t C.Map.t;
  children : C.Set.t C.Map.t;
  ancestors : Rel.Invertible.t; (* i.e. dominators *)
  frontiers : C.Set.t C.Map.t Lazy.t;
  successors : Rel.Invertible.t;
}

let root_node t = t.root_node
let parent t k = C.Map.find_opt k t.parents
let children t k = Rel.find k t.children
let ancestors t k = Rel.find k (Rel.Invertible.forward t.ancestors)
let descendants t k = Rel.find k (Rel.Invertible.backward t.ancestors)
let dominance_frontier t k = Rel.find k (Lazy.force t.frontiers)
let successors t k = Rel.find k (Rel.Invertible.forward t.successors)
let predecessors t k = Rel.find k (Rel.Invertible.backward t.successors)

let iter f t =
  let rec loop k =
    let child_nodes = children t k in
    f k child_nodes;
    C.Set.iter loop child_nodes;
  in
  loop t.root_node

let calculate_frontiers ~root_node ~parents ~children ~ancestors ~successors =
  (* Adapted from: Cytron et al. (1991). "Efficiently computing static single assignment
     form and the control dependence graph." *)
  let successors = Rel.Invertible.forward successors in
  let ancestors = Rel.Invertible.forward ancestors in
  let dominates k1 k2 = Rel.mem k2 k1 ancestors in
  let strictly_dominates k1 k2 = not (C.equal k1 k2) && dominates k1 k2 in
  let rec frontiers k =
    let succs_of_k = Rel.find k successors in
    let children_of_k = Rel.find k children |> C.Set.elements in
    let df_local =
      C.Set.filter (fun succ -> not (strictly_dominates k succ)) succs_of_k
    in
    let frontiers_of_children, maps_of_children =
      List.map frontiers children_of_k
      |> List.split
    in
    let df_up_of_children =
      List.map (fun frontier ->
        C.Set.filter (fun y ->
          match C.Map.find_opt y parents with
          | Some parent -> not (C.equal parent k)
          | None -> true
        ) frontier
      ) frontiers_of_children
      |> List.fold_left C.Set.union C.Set.empty
    in
    let frontier = C.Set.union df_local df_up_of_children in
    (* Format.eprintf "df_local of %a:@.%a@.df_up of children of %a:@.%a@."
     *   C.print k
     *   C.Set.print df_local
     *   C.print k
     *   C.Set.print df_up_of_children; *)
    let map =
      List.fold_left Rel.union_disjoint_exn C.Map.empty maps_of_children
    in
    frontier, C.Map.add k frontier map
  in
  let _, frontiers = frontiers root_node in
  frontiers

let dominance_frontier_of_set t set =
  let frontiers = List.map (dominance_frontier t) (set |> C.Set.elements) in
  List.fold_left C.Set.union C.Set.empty frontiers

let iterated_dominance_frontier t orig_set =
  let rec loop set =
    let next = dominance_frontier_of_set t (C.Set.union set orig_set) in
    if C.Set.equal next set
    then set
    else loop next
  in loop orig_set

let from_dom_sets ~root_node ~(dom : C.Set.t C.Map.t) ~(successors : Rel.Invertible.t) =
  let max_dominators k1 k2 =
    match k2 with
    | None ->
      Some k1
    | Some k2 ->
      let desc1 = C.Map.find k1 dom in
      let desc2 = C.Map.find k2 dom in
      if C.Set.cardinal desc1 > C.Set.cardinal desc2 then Some k1 else Some k2
  in
  let parents : C.t C.Map.t =
    C.Map.fold (fun desc ancs acc ->
      let strict_ancs = C.Set.remove desc ancs in
      (* Whichever strict dominator itself has the most dominators is the parent *)
      match C.Set.fold max_dominators strict_ancs None with
      | None -> acc
      | Some parent -> C.Map.add desc parent acc
    ) dom C.Map.empty
  in
  let children =
    C.Map.fold (fun child parent children ->
      Rel.add parent child children
    ) parents C.Map.empty
  in
  assert (C.Map.for_all (fun parent children ->
    C.Set.for_all (fun child ->
      C.equal parent (C.Map.find child parents)
    ) children
  ) children);
  let ancestors = Rel.Invertible.of_relation dom in
  let frontiers = lazy (
    calculate_frontiers ~root_node ~parents ~children ~ancestors ~successors
  ) in
  { root_node; parents; children; successors; ancestors; frontiers }

let of_successor_relation ~root_node (rel : Rel.Invertible.t) : t =
  let preds = Rel.Invertible.backward rel in
  let all_nodes =
    C.Map.bindings preds
    |> List.map (fun (a, b) -> a :: C.Set.elements b)
    |> List.flatten
    |> C.Set.of_list
  in
  let initial_dom =
    C.Map.of_set (fun node ->
      if C.equal node root_node then C.Set.singleton root_node else all_nodes
    ) all_nodes
  in
  let rec go dom =
    let changed = ref false in
    let dom =
      C.Map.fold (fun node preds dom ->
        let new_set_without_node =
          C.Set.fold (fun pred acc ->
            let dom_of_pred = C.Map.find pred dom in
            match acc with
            | None -> Some dom_of_pred
            | Some acc -> Some (C.Set.inter dom_of_pred acc)
          ) preds None
        in
        let new_set =
          match new_set_without_node with
          | None -> C.Set.singleton node (* no predecessors *)
          | Some set -> C.Set.add node set
        in
        if C.Set.equal new_set (C.Map.find node dom)
        then dom
        else (changed := true; C.Map.add node new_set dom)
      ) preds dom
    in
    if !changed then go dom else dom
  in
  let dom = go initial_dom in
  from_dom_sets ~root_node ~dom ~successors:rel

let of_successor_map ~root_node map =
  of_successor_relation ~root_node (Rel.Invertible.of_relation map)

let succ_rel ~root_node (body : Ilambda.t) : Rel.Invertible.t =
  let combine (rel1, succs1) (rel2, succs2) =
    (Rel.union_disjoint_exn rel1 rel2,
     C.Set.union succs1 succs2)
  in
  (* Returns a successor relation and the successors of the current continuation *)
  let rec succs (body : Ilambda.t) : Rel.t * C.Set.t =
    match body with
    | Ilambda.Let (_, named, body) -> combine (succs_named named) (succs body)
    | Let_mutable { body; _ } -> succs body
    | Let_rec (_, body) -> succs body
    | Let_cont { name; handler; body } ->
      let handler_map, handler_succs = succs handler in
      let body_map, body_succs = succs body in
      let map =
        Rel.union_disjoint_exn handler_map body_map
        |> C.Map.add name handler_succs
      in
      map, body_succs
    | Apply { continuation = k1; exn_continuation = k2 } ->
      C.Map.empty,
      C.Set.add k1 (C.Set.singleton k2)
    | Apply_cont (k, _, _) -> C.Map.empty, C.Set.singleton k
    | Switch (_, { consts; failaction; _ }) ->
      let succs =
        (match failaction with Some k -> [k] | None -> [])
        @ List.map snd consts
        |> C.Set.of_list
      in
      C.Map.empty, succs
    | Event (body, _) -> succs body
  and succs_named (named : Ilambda.named) : Rel.t * C.Set.t =
    match named with
    | Prim { exception_continuation = k; _ } ->
      C.Map.empty, C.Set.singleton k
    | _ ->
      C.Map.empty, C.Set.empty
  in
  let map, succs = succs body in
  C.Map.add root_node succs map
  |> Rel.Invertible.of_relation

let of_ilambda ~root_node (body : Ilambda.t) : t =
  of_successor_relation ~root_node (succ_rel ~root_node body)

let of_function_declaration (fun_decl : Ilambda.function_declaration) =
  let root_node = C.create () in
  of_ilambda ~root_node (fun_decl.body)

let print ppf t =
  let rec print_from ppf node =
    let child_nodes = children t node in
    let is_leaf node = C.Set.is_empty (children t node) in
    if C.Set.is_empty child_nodes
    then C.print ppf node
    else
      let print_leaf_children ppf leaves =
        let pp_comma ppf () = Format.fprintf ppf ",@ " in
        match leaves with
        | [] ->
          ()
        | _ ->
          Format.fprintf ppf "+-- @[<hov 2>%a@]"
            (Format.pp_print_list C.print ~pp_sep:pp_comma) leaves
      in
      let print_subtree ppf child = Format.fprintf ppf "+-- %a" print_from child in
      let print_non_leaf_children ppf non_leaves =
        Format.pp_print_list print_subtree ppf non_leaves
      in
      let print_children ppf () =
        let leaves, non_leaves = List.partition is_leaf (child_nodes |> C.Set.elements) in
        print_leaf_children ppf leaves;
        begin
          match leaves, non_leaves with
          | _ :: _, _ :: _ -> Format.fprintf ppf "@,"
          | _, _ -> ()
        end;
        print_non_leaf_children ppf non_leaves;
      in
      Format.fprintf ppf "@[<v>%a@,%a@]" C.print node print_children ()
  in
  print_from ppf t.root_node

let print_dominance_frontiers ppf t =
  Rel.print ppf (Lazy.force t.frontiers)
