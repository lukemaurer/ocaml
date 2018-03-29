(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2018 OCamlPro SAS                                    *)
(*   Copyright 2014--2018 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module Env = Closure_conversion_aux.Env
module Function_decls = Closure_conversion_aux.Function_decls
module Function_decl = Function_decls.Function_decl
module Program_body = Flambda_static.Program_body
module Typed_parameter = Flambda.Typed_parameter

module K = Flambda_kind
module P = Flambda_primitive
module T = Flambda_type

type t = {
  current_unit_id : Ident.t;
  symbol_for_global' : (Ident.t -> Symbol.t);
  filename : string;
  mutable imported_symbols : Symbol.Set.t;
  (* All symbols in [imported_symbols] are to be of kind [Value]. *)
  mutable declared_symbols :
    (Symbol.t * Flambda_static0.Static_part.t) list;
}

let assigning_conts ~root_node (fun_decl : Ilambda.function_declaration)
  : Continuation.Set.t Ident.Map.t =
  let combine_maps map1 map2 =
    (* The map should only be added to at a definition site, so there should be
       no duplicates *)
    Continuation.Map.union (fun _ _ _ -> assert false) map1 map2
  in
  let combine_assigned = Ident.Set.union in
  let combine_pairs (m1, a1) (m2, a2) = (combine_maps m1 m2, combine_assigned a1 a2) in

  let rec go (body : Ilambda.t) : Ident.Set.t Continuation.Map.t * Ident.Set.t =
    match body with
    | Let (_, named, body) -> combine_pairs (go_named named) (go body)
    | Let_mutable { id; body; _ } ->
      let map, assigned = go body in
      (* This counts as an assignment *)
      map, Ident.Set.add id assigned
    | Let_rec (_, body) -> go body (* only interested in this function *)
    | Let_cont { name; handler; body } ->
      let handler_map, handler_assigned = go handler in
      let body_map, body_assigned = go body in
      let map =
        combine_maps body_map (Continuation.Map.add name handler_assigned handler_map)
      in
      map, body_assigned
    | Event (body, _) -> go body
    | Apply _ | Apply_cont _ | Switch _ -> Continuation.Map.empty, Ident.Set.empty
  and go_named (named : Ilambda.named) = match named with
    | Assign { being_assigned; _ } ->
      Continuation.Map.empty, Ident.Set.singleton being_assigned
    | Var _ | Const _ | Prim _ ->
      Continuation.Map.empty, Ident.Set.empty
  in
  let map, assigned = go fun_decl.body in
  let map = Continuation.Map.add root_node assigned map in
  let module Rel = Relation.Make(Continuation)(Ident) in
  Rel.inverse map

(* In CFG language, a phi-node is a variable with a map from each predecessor to a value
   for the variable. For us, a phi-node is a new argument to be added to a continuation,
   with a map from each continuation that invokes it to the value it should pass.

   (See Maurer, L. (2018). "The Design of Intermediate Languages in Optimizing
   Compilers.")
*)
module Phi_node = struct
  type t = Ident.t ref Continuation.Map.t
end

module Phi_node_map = struct
  type t = (Ident.t ref * Phi_node.t) list Continuation.Map.t

  let print ppf t =
    Continuation.Map.print (Format.pp_print_list ?pp_sep:None (fun ppf (v, map) ->
      Format.fprintf ppf "%a <-- %a"
        Ident.print !v
        (Continuation.Map.print (fun ppf v -> Ident.print ppf !v)) map
    )) ppf t

  let _ = print
end

let iter_let_cont f (term : Ilambda.t) =
  let rec go (term : Ilambda.t) =
    match term with
    | Let (_, _, body) -> go body
    | Let_mutable { body; _ } -> go body
    | Let_rec (_, body) -> go body
    | Let_cont let_cont -> f let_cont; go let_cont.handler; go let_cont.body
    | Event (body, _) -> go body
    | Apply _ | Apply_cont _ | Switch _ -> ()
  in
  go term

(* let make_ast_match_dom_tree (fun_decl : Ilambda.function_declaration) dom_tree =
 *   Format.eprintf "BEFORE:@.%a@." Ilambda.print_function fun_decl;
 *   let cont_decls : Ilambda.let_cont Continuation.Tbl.t = Continuation.Tbl.create 10 in
 *   let dummy_ident = Ident.create "*deleted body*" in
 *   let dummy_body = Ilambda.Switch (dummy_ident, {
 *     numconsts = 0; consts = []; failaction = None
 *   }) in
 *   iter_let_cont (fun let_cont ->
 *     Continuation.Tbl.add cont_decls let_cont.name
 *       ({ let_cont with body = dummy_body } : Ilambda.let_cont)
 *   ) fun_decl.body;
 *
 *   Format.eprintf "Continuations:@.%a@.Dom tree:@.%a@."
 *     Continuation.Set.print
 *     (cont_decls |> Continuation.Tbl.to_map |> Continuation.Map.keys)
 *     Dominator_tree.print dom_tree;
 *
 *   let rec rewrite (term : Ilambda.t) : Ilambda.t =
 *     match term with
 *     | Let (id, named, body) ->
 *       Let (id, named, rewrite body)
 *     | Let_mutable let_mutable ->
 *       Let_mutable { let_mutable with body = rewrite let_mutable.body }
 *     | Let_rec (decls, body) ->
 *       Let_rec (decls, rewrite body)
 *     | Let_cont { body; _ } ->
 *       rewrite body
 *     | Event (body, event) ->
 *       Event (rewrite body, event)
 *     | Apply _ | Apply_cont _ | Switch _ ->
 *       term
 *   and rewrite_handler k term : Ilambda.t =
 *     let term = rewrite term in
 *     let children = Dominator_tree.children dom_tree k |> Continuation.Set.elements in
 *     let child_decls = List.fold_left (fun decls cont ->
 *       match Continuation.Tbl.find_opt cont_decls cont with
 *       | Some decl -> decl :: decls
 *       | None -> decls) [] children
 *     in
 *     List.fold_left (fun body (decl : Ilambda.let_cont) ->
 *       Ilambda.Let_cont
 *         { decl with body; handler = rewrite_handler decl.name decl.handler }
 *     ) term child_decls
 *   in
 *   let body = rewrite_handler (Dominator_tree.root_node dom_tree) fun_decl.body in
 *   (fun ans -> Format.eprintf "AFTAIR:@.%a@." Ilambda.print_function ans; ans) @@
 *   ({ fun_decl with body } : Ilambda.function_declaration)
 * ;; *)

let place_phi_nodes
      ~(dom_tree : Dominator_tree.t)
      ~(assigning_conts : Continuation.Set.t Ident.Map.t)
      (fun_decl : Ilambda.function_declaration)
  : Phi_node_map.t =
    (* Sadly, we can't use the iterated dominance frontier, at least not trivially,
       because we need a phi-node in either {i two} situations: (1) the usual one, a
       join point where something can take multiple values, and (2) where otherwise the
       variable would go out of scope. SSA has a flat scope, so it avoids the issue that
       way. We {i could} fix this by rearranging the CPS term so that its structure
       matches the dominator tree (that this is possible is a corollary of the
       interderivability of CPS and SSA; in particular, CPS->SSA uses the dominator tree,
       so we could translate to SSA and back (!)), but putting everything in just the
       right place would be hard.

       For the moment, we punt by just putting a phi-node anywhere a mutable variable is
       in scope. *)
    (* (* Briggs et al.: "Place a phi-node for [v] in the iterated dominance frontier
     *    of [A(v)]." *)
     * Ident.Map.fold (fun v ks phi_nodes ->
     *   (* XX lmaurer: This almost certainly won't be fast enough in real life; real
     *      implementations don't construct the IDF explicitly. *)
     *   let idf : Continuation.Set.t =
     *     Dominator_tree.iterated_dominance_frontier dom_tree ks
     *   in
     *   Format.eprintf "Iterated dominance frontier of %a:@.%a@."
     *     Continuation.Set.print ks
     *     Continuation.Set.print idf;
     *   Continuation.Set.fold (fun k phi_nodes ->
     *     if is_exit_node k
     *     then phi_nodes
     *     else
     *       (* Each phi-node starts out as v <- (v, ..., v) *)
     *       let new_phi_node =
     *         let predecessors = Dominator_tree.predecessors dom_tree k in
     *         Continuation.Set.fold (fun pred phi_node ->
     *           Continuation.Map.add pred (ref v) phi_node
     *         ) predecessors Continuation.Map.empty
     *       in
     *       Continuation.Map.update k (function
     *         | Some phi_nodes_for_k -> Some ((ref v, new_phi_node) :: phi_nodes_for_k)
     *         | None -> Some [(ref v, new_phi_node)]
     *       ) phi_nodes
     *   ) idf phi_nodes
     * ) assigning Continuation.Map.empty *)
  let _ = assigning_conts in

  let conts_in_scope : Continuation.Set.t Ident.Tbl.t =
    Ident.Tbl.create 10
  in

  (* Return continuations defined in subtree, recording the result at
     each [Let_mutable] *)
  let rec go (body : Ilambda.t) : Continuation.Set.t =
    match body with
    | Let (_, _, body) -> go body
    | Let_mutable { id; body; _ } ->
      let conts = go body in
      Ident.Tbl.add conts_in_scope id conts;
      conts
    | Let_rec (_, body) -> go body (* only interested in this function *)
    | Let_cont { name; handler; body } ->
      let handler_conts = go handler in
      let body_conts = go body in
      Continuation.Set.add name (Continuation.Set.union handler_conts body_conts)
    | Event (body, _) -> go body
    | Apply _ | Apply_cont _ | Switch _ -> Continuation.Set.empty
  in
  ignore (go fun_decl.body : Continuation.Set.t);
  let ids_for_cont : Ident.Set.t Continuation.Map.t =
    let module Rel = Relation.Make(Ident)(Continuation) in
    Rel.inverse (conts_in_scope |> Ident.Tbl.to_map)
  in

  Continuation.Map.mapi (fun k ids_in_scope ->
    List.map (fun v ->
      (* Each phi-node starts out as v <- (v, ..., v) *)
      let new_phi_node =
        let predecessors = Dominator_tree.predecessors dom_tree k in
        Continuation.Set.fold (fun pred phi_node ->
          Continuation.Map.add pred (ref v) phi_node
        ) predecessors Continuation.Map.empty
      in
      (ref v, new_phi_node)
    ) (ids_in_scope |> Ident.Set.elements)
  ) ids_for_cont

let remove_mutable_variables (fun_decl : Ilambda.function_declaration)
  : Ilambda.function_declaration =
  let is_exit_node k =
    Continuation.equal k fun_decl.continuation_param
    || Continuation.equal k fun_decl.exn_continuation_param
  in
  (* Adapted from: Briggs, Cooper, Harvey, and Simpson (1998). "Practical improvements to
     the Construction and Destruction of Static Single Assignment Form." *)
  let dom_tree = Dominator_tree.of_function_declaration fun_decl in
  (* Format.eprintf "Dominance tree:@.%a@.Dominance frontiers:@.%a@."
   *   Dominator_tree.print dom_tree
   *   Dominator_tree.print_dominance_frontiers dom_tree; *)
  (* let fun_decl = make_ast_match_dom_tree fun_decl dom_tree in *)
  let root_node = Dominator_tree.root_node dom_tree in
  (* Briggs et al.: "[A(v) <- {blocks containing an assignment to v}]" *)
  let assigning : Continuation.Set.t Ident.Map.t = assigning_conts ~root_node fun_decl in
  (* Format.eprintf "Continuations that assign:@.%a@."
   *   (Ident.Map.print Continuation.Set.print) assigning; *)
  let phi_nodes : Phi_node_map.t =
    place_phi_nodes ~assigning_conts:assigning ~dom_tree fun_decl
  in
  (* Format.eprintf "Placing phi-nodes in continuations:@.%a@."
   *   Continuation.Set.print (Continuation.Map.keys phi_nodes); *)
  let find_phi_nodes k =
    match Continuation.Map.find_opt k phi_nodes with
    | Some nodes -> nodes
    | None -> []
  in

  (* The paper uses a list called [counters] to keep track of the next fresh name to
     assign to each variable. We have our own way of freshening, so we store the fresh
     names directly in [stacks] instead of messing about with integers. *)
  let stacks : Ident.t list ref Ident.Map.t = Ident.Map.map (fun _ -> ref []) assigning in
  let push v_new v =
    let stack = Ident.Map.find v stacks in
    stack := v_new :: !stack
  in
  let pop v =
    let stack = Ident.Map.find v stacks in
    match !stack with
    | _ :: tail -> stack := tail
    | [] -> assert false
  in
  let top v =
    let stack = Ident.Map.find v stacks in
    match !stack with
    | top :: _ -> top
    | [] -> assert false
  in

  (* Rename the body and populate the phi-nodes. The name [search] is from the paper.
     In lieu of in-place modification, builds a map from each continuation name to the
     new version of it (where letcont bodies are meaningless). The next pass does the
     modifications. *)
  let orig_bodies = Continuation.Tbl.create 10 in
  Continuation.Tbl.add orig_bodies root_node fun_decl.body;
  iter_let_cont (fun { name; handler; _ } ->
    Continuation.Tbl.add orig_bodies name handler
  ) fun_decl.body;
  let new_bodies = Continuation.Tbl.create 10 in
  (* Remember the parameter list of each continuation so we can make stubs. *)
  let param_lists = Continuation.Tbl.create 10 in
  Continuation.Tbl.add param_lists root_node [];
  let rec search k body =
    let phis = find_phi_nodes k in
    (* At the end, we're going to pop everything we've pushed, so track it *)
    let to_pop = ref [] in
    let push_fresh v =
      to_pop := v :: !to_pop;
      let v_new = Ident.rename v in
      push v_new v;
      v_new
    in
    List.iter (fun (v, _map) -> v := push_fresh !v) phis;
    (* NOTE: We only need to rewrite {i mutable} variables. *)
    let rec rewrite (term : Ilambda.t) : Ilambda.t =
      match term with
      | Let (id, Prim { prim = Pread_mutable var; _ }, body) ->
        let var = top var in
        let body = rewrite body in
        Let (id, Var var, body)
      | Let (id, Assign { being_assigned; new_value }, body) ->
        let new_var = push_fresh being_assigned in
        Let (new_var, Var new_value,
             Let (id, Const Lambda.const_unit,
                  rewrite body))
      | Let (id, other_named, body) ->
        Let (id, other_named, rewrite body)
      | Let_mutable { id; initial_value; body; _ } ->
        let id = push_fresh id in
        Let (id, Var initial_value, rewrite body)
      | Let_rec (decls, body) ->
        (* Functions don't have free references to mutable variables, so we don't need to
           rewrite the declarations (hooray!). *)
        Let_rec (decls, rewrite body)
      | Let_cont ({ name; params; body; _ } as let_cont) ->
        Continuation.Tbl.add param_lists name params;
        (* Handler will be replaced in next pass *)
        Let_cont { let_cont with body = rewrite body }
      | Event (body, event) ->
        Event (rewrite body, event)
      | Apply _ | Apply_cont _ | Switch _ ->
        term
    in
    let new_body = rewrite body in
    Continuation.Tbl.add new_bodies k new_body;
    let succs = Dominator_tree.successors dom_tree k in
    Continuation.Set.iter (fun succ ->
      let phis = find_phi_nodes succ in
      List.iter (fun (_, phi) ->
        let arg = Continuation.Map.find k phi in
        (* The node starts out as [v <- phi(v, v, ..., v)], but the first v has been
           renamed, so use the argument *)
        arg := top !arg;
      ) phis
    ) succs;
    let children = Dominator_tree.children dom_tree k in
    Continuation.Set.iter (fun child ->
      match Continuation.Tbl.find_opt orig_bodies child with
      | Some body -> search child body
      | None -> assert (is_exit_node child) (* a parameter, i.e., an exit node *)
    ) children;
    List.iter pop !to_pop;
  in
  search root_node fun_decl.body;
  (* CR lmaurer: Could probably do the next two loops at once. *)
  let rec replace_bodies (term : Ilambda.t) : Ilambda.t =
    match term with
    | Let (id, named, body) ->
      Let (id, named, replace_bodies body)
    | Let_mutable _ ->
      assert false (* these should be gone! *)
    | Let_rec (decls, body) ->
      Let_rec (decls, replace_bodies body)
    | Let_cont ({ name; body; _ } as let_cont) ->
      Let_cont {
        let_cont with
        handler = replace_bodies (Continuation.Tbl.find new_bodies name);
        body = replace_bodies body;
      }
    | Event (body, event) ->
      Event (replace_bodies body, event)
    | Apply _ | Apply_cont _ | Switch _ ->
      term
  in
  let body = replace_bodies (Continuation.Tbl.find new_bodies root_node) in

  (* Finally, implement the phi-nodes. *)
  let new_args ~(caller : Continuation.t) cont : Ident.t list =
    let phis = find_phi_nodes cont in
    List.map (fun (_, args_by_caller) ->
      !(Continuation.Map.find caller args_by_caller)
    ) phis
  in

  let replace_cont_with_stub ~is_exn_handler ~(caller : Continuation.t)
        (cont : Continuation.t)
    : Continuation.t * (Ilambda.t -> Ilambda.t) =
    (* CR-soon lmaurer: Could be caching stubs by arguments. *)
    if is_exit_node cont
    then cont, fun body -> body
    else
      match find_phi_nodes cont with
      | [] -> cont, fun body -> body
      | phis ->
        let params =
          Continuation.Tbl.find param_lists cont
          |> List.map Ident.rename (* keep everything unique *)
        in
        let new_param_vals =
          List.map (fun (_, args_by_predecessor) ->
            !(Continuation.Map.find caller args_by_predecessor)
          ) phis
        in
        let new_cont = Continuation.create () in
        let handler = Ilambda.Apply_cont (cont, None, params @ new_param_vals) in
        new_cont, fun body ->
          Let_cont {
            name = new_cont;
            recursive = Nonrecursive;
            administrative = false;
            params; handler; is_exn_handler; body;
          }
  in

  let rec add_phis k (term : Ilambda.t) : Ilambda.t =
    match term with
    | Let (id, named, body) ->
      let named, bind_new_things = add_phis_named k named in
      bind_new_things (Ilambda.Let (id, named, add_phis k body))
    | Let_mutable _ ->
      assert false
    | Let_rec (decls, body) ->
      Let_rec (decls, add_phis k body)
    | Let_cont ({ name; handler; body; params; is_exn_handler; _ } as let_cont) ->
      let phi_nodes = find_phi_nodes name in
      let new_params = List.map (fun (v, _) -> !v) phi_nodes in
      (* If it has phi-nodes, it's no longer an exception handler (and all references to
         it go through shims that {i are} exception handlers). *)
      let is_exn_handler = match phi_nodes with
        | [] -> is_exn_handler
        | _ :: _ -> false
      in
      Let_cont {
        let_cont with
        is_exn_handler;
        params = params @ new_params;
        handler = add_phis name handler;
        body = add_phis k body;
      }
    | Apply ({ continuation; exn_continuation; _ } as apply) ->
      let new_cont, bind_cont =
        replace_cont_with_stub continuation ~caller:k ~is_exn_handler:false
      in
      let new_exn_cont, bind_exn_cont =
        replace_cont_with_stub exn_continuation ~caller:k ~is_exn_handler:true
      in
      bind_cont (bind_exn_cont (Apply {
        apply with
        continuation = new_cont;
        exn_continuation = new_exn_cont;
      }))
    | Apply_cont (cont, trap, args) ->
      begin
        match new_args ~caller:k cont with
        | [] -> term
        | new_args -> Apply_cont (cont, trap, args @ new_args)
      end
    | Event (body, event) ->
      Event (add_phis k body, event)
    | Switch (id, ({ consts; failaction; _ } as switch)) ->
      let new_consts_and_binders
        : ((int * Continuation.t) * (Ilambda.t -> Ilambda.t)) list =
        List.map (fun (tag, cont) ->
          let new_cont, bind_cont =
            replace_cont_with_stub cont ~caller:k ~is_exn_handler:false
          in
          (tag, new_cont), bind_cont
        ) consts
      in
      let failaction, bind_failaction =
        match failaction with
        | Some failaction ->
          let new_cont, bind_cont =
            replace_cont_with_stub failaction ~caller:k ~is_exn_handler:false
          in
          Some new_cont, bind_cont
        | None ->
          None, fun body -> body
      in
      let bind_branches body =
        List.fold_left (fun body (_, bind) -> bind body)
          body new_consts_and_binders
      in
      bind_branches @@
      bind_failaction @@
      Ilambda.Switch (id, {
        switch with failaction; consts = List.map fst new_consts_and_binders
      })
  (* Returns both a new named and a function to run on the body to bind any
     stubs needed *)
  and add_phis_named k (named : Ilambda.named)
    : Ilambda.named * (Ilambda.t -> Ilambda.t) =
    match named with
    | Prim { prim; args; loc; exception_continuation } ->
      let exception_continuation, bind_cont =
        replace_cont_with_stub exception_continuation ~caller:k ~is_exn_handler:true
      in
      Prim { prim; args; loc; exception_continuation }, bind_cont
    | _ ->
      named, fun body -> body
  in
  let body = add_phis root_node body in
  (fun (ans : Ilambda.function_declaration) ->
     Format.eprintf "Body without mutables:@.%a@." Ilambda.print_function ans; ans) @@
  ({ fun_decl with body } : Ilambda.function_declaration)
;;

(** Generate a wrapper ("stub") function that accepts a tuple argument and
    calls another function with arguments extracted in the obvious
    manner from the tuple. *)
let tupled_function_call_stub
      ( original_params : ( Variable.t * Lambda.value_kind ) list )
      (unboxed_version : Closure_id.t) ~(closure_bound_var : Closure_id.t)
      : Flambda.Function_declaration.t =
  let continuation_param = Continuation.create () in
  let exn_continuation_param = Continuation.create () in
  let tuple_param_var =
    Variable.rename ~append:"tupled_stub_param"
      (Closure_id.unwrap unboxed_version)
  in
  let my_closure =
    Variable.rename ~append:"tupled_stub"
      (Closure_id.unwrap unboxed_version)
  in
  let params = List.map (fun (p, _) -> Variable.rename p) original_params in
  let unboxed_version_var =
    Variable.create "unboxed_version"
  in
  let call : Flambda.Expr.t =
    Apply ({
        continuation = continuation_param;
        exn_continuation = exn_continuation_param;
        func = Name.var unboxed_version_var;
        args = List.map Simple.var params;
        (* CR-someday mshinwell for mshinwell: investigate if there is some
           redundancy here (func is also unboxed_version) *)
        call_kind = Function (Direct {
          closure_id = unboxed_version;
          return_arity = [K.value ()];
        });
        dbg = Debuginfo.none;
        inline = Default_inline;
        specialise = Default_specialise;
      })
  in
  let body_with_closure_bound =
    let move =
      P.Move_within_set_of_closures {
        move_from = closure_bound_var;
        move_to = unboxed_version;
      }
    in
    Flambda.Expr.create_let unboxed_version_var
      (K.value ())
      (Prim (Unary (move, Simple.var my_closure), Debuginfo.none))
      call
  in
  let _, body =
    List.fold_left (fun (pos, body) param ->
        let lam : Flambda.Named.t =
          let pos = Immediate.int (Targetint.OCaml.of_int pos) in
          Prim (Binary (Block_load (Block (Value Unknown), Immutable),
                        Simple.var tuple_param_var,
                        Simple.const (Tagged_immediate pos)),
                Debuginfo.none)
        in
        pos + 1,
        Flambda.Expr.create_let param (K.value ()) lam body)
      (0, body_with_closure_bound) params
  in
  let tuple_param =
    (* We do not have an accessor here *)

    (* Flambda.Typed_parameter.create (Parameter.wrap tuple_param_var) *)
    (*   (Flambda_type.block Tag.Scannable.zero *)
    (*     (Array.of_list *)
    (*       (List.map (fun _ -> Flambda_type.any_value Must_scan Other) params))) *)

    Flambda.Typed_parameter.create (Parameter.wrap tuple_param_var)
      (T.any_value ())
  in
  Flambda.Function_declaration.create
    ~my_closure
    ~params:[tuple_param] ~continuation_param
    ~exn_continuation_param
    ~return_arity:[K.value ()]
    ~body ~stub:true ~dbg:Debuginfo.none ~inline:Default_inline
    ~specialise:Default_specialise ~is_a_functor:false
    ~closure_origin:(Closure_origin.create closure_bound_var)

module Static_part = Flambda_static0.Static_part

let register_const t (constant : Static_part.t) name
      : Flambda_static0.Of_kind_value.t * string =
  let current_compilation_unit = Compilation_unit.get_current_exn () in
  (* Create a variable to ensure uniqueness of the symbol *)
  let var = Variable.create ~current_compilation_unit name in
  let symbol =
    Flambda_utils.make_variable_symbol var
  in
  t.declared_symbols <- (symbol, constant) :: t.declared_symbols;
  Symbol symbol, name

let rec declare_const t (const : Lambda.structured_constant)
      : Flambda_static0.Of_kind_value.t * string =
  match const with
  | Const_base (Const_int c) ->
    Tagged_immediate (Immediate.int (Targetint.OCaml.of_int c)), "int"
  | Const_base (Const_char c) -> Tagged_immediate (Immediate.char c), "char"
  | Const_base (Const_string (s, _)) ->
    let const, name =
      (* CR mshinwell: Double-check this is the correct condition for
         everything in the application being compiled with safe-string *)
      if Config.safe_string then
        Static_part.Immutable_string (Const s), "immstring"
      else
        Static_part.Mutable_string { initial_value = Const s; },
          "string"
    in
    register_const t const name
  | Const_base (Const_float c) ->
    let c = Numbers.Float_by_bit_pattern.create (float_of_string c) in
    register_const t (Static_part.Boxed_float (Const c)) "float"
  | Const_base (Const_int32 c) ->
    register_const t (Static_part.Boxed_int32 (Const c)) "int32"
  | Const_base (Const_int64 c) ->
    register_const t (Static_part.Boxed_int64 (Const c)) "int64"
  | Const_base (Const_nativeint c) ->
    (* CR pchambart: this should be pushed further to lambda *)
    let c = Targetint.of_int64 (Int64.of_nativeint c) in
    register_const t (Static_part.Boxed_nativeint (Const c))
      "nativeint"
  | Const_immstring c ->
    register_const t (Static_part.Immutable_string (Const c)) "immstring"
  | Const_float_array c ->
    (* CR mshinwell: check that Const_float_array is always immutable *)
    register_const t
      (Static_part.Immutable_float_array
         (List.map (fun s ->
           let f = float_of_string s in
           let f = Numbers.Float_by_bit_pattern.create f in
           Static_part.Const f) c))
      "float_array"
  | Const_block (tag, consts) ->
    let const : Static_part.t =
      Block
        (Tag.Scannable.create_exn tag, Immutable,
         List.map (fun c -> fst (declare_const t c)) consts)
    in
    register_const t const "const_block"

let close_const t (const : Lambda.structured_constant)
      : Flambda.Named.t * string =
  match declare_const t const with
  | Tagged_immediate c, name ->
    Simple (Simple.const (Tagged_immediate c)), name
  | Symbol s, name ->
    Simple (Simple.symbol s), name
  | Dynamically_computed _, name ->
    Misc.fatal_errorf "Declaring a computed constant %s" name

(* CR pchambart: move to flambda_type ? *)
let flambda_type_of_lambda_value_kind (k : Lambda.value_kind) : Flambda_type.t =
  match k with
  | Pgenval ->
    Flambda_type.any_value ()
  | Pfloatval ->
    Flambda_type.any_boxed_float ()
  | Pboxedintval Pint32 ->
    Flambda_type.any_boxed_int32 ()
  | Pboxedintval Pint64 ->
    Flambda_type.any_boxed_int64 ()
  | Pboxedintval Pnativeint ->
    Flambda_type.any_boxed_nativeint ()
  | Pintval ->
    Flambda_type.any_tagged_immediate ()
  | Pnaked_intval ->
    Misc.fatal_error "[Pnaked_intval] shouldn't exist before Flambda"

let convert_inline_attribute_from_lambda
      (attr : Lambda.inline_attribute)
      : Flambda.inline_attribute =
  match attr with
  | Always_inline -> Always_inline
  | Never_inline -> Never_inline
  | Unroll i -> Unroll i
  | Default_inline -> Default_inline

let convert_specialise_attribute_from_lambda
      (attr : Lambda.specialise_attribute)
      : Flambda.specialise_attribute =
  match attr with
  | Always_specialise -> Always_specialise
  | Never_specialise -> Never_specialise
  | Default_specialise -> Default_specialise

let kind_of_repr (repr : Primitive.native_repr) : K.t =
  match repr with
  | Same_as_ocaml_repr -> K.value ()
  | Unboxed_float -> K.naked_float ()
  | Unboxed_integer Pnativeint -> K.naked_nativeint ()
  | Unboxed_integer Pint32 -> K.naked_int32 ()
  | Unboxed_integer Pint64 -> K.naked_int64 ()
  | Untagged_int -> K.naked_immediate ()

let rec close t env (lam : Ilambda.t) : Flambda.Expr.t =
  match lam with
  | Let (id,
         Prim { prim = Pccall prim; args; loc; exception_continuation },
         body) ->
    (* CR pchambart: there should be a special case if body is a
       apply_cont *)
    let continuation = Continuation.create () in
    let return_kind = kind_of_repr prim.prim_native_repr_res in
    let call_kind : Flambda.Call_kind.t =
      C_call {
        alloc = prim.prim_alloc;
        param_arity = List.map kind_of_repr prim.prim_native_repr_args;
        return_arity = [ return_kind ];
      }
    in
    let call_symbol =
      Symbol.create
        (Compilation_unit.external_symbols ())
        (Linkage_name.create prim.prim_name)
    in
    let dbg = Debuginfo.from_location loc in
    (* TODO:
       unbox arguments
       box return *)
    let call args : Flambda.Expr.t =
      Apply ({
        call_kind;
        func = Name.symbol call_symbol;
        args;
        continuation;
        exn_continuation = exception_continuation;
        dbg;
        inline = Default_inline;
        specialise = Default_specialise;
      })
    in
    let call =
      List.fold_right2
        (fun arg (arg_repr:Primitive.native_repr)
          (call : Simple.t list -> Flambda.Expr.t) ->
        let boxing : P.unary_primitive option =
          match arg_repr with
          | Same_as_ocaml_repr -> None
          | Unboxed_float ->
            Some (P.Unbox_number Naked_float)
          | Unboxed_integer Pnativeint ->
            Some (P.Unbox_number Naked_nativeint)
          | Unboxed_integer Pint32 ->
            Some (P.Unbox_number Naked_int32)
          | Unboxed_integer Pint64 ->
            Some (P.Unbox_number Naked_int64)
          | Untagged_int ->
            Some (P.Num_conv { src = Tagged_immediate; dst = Naked_nativeint; })
        in
        match boxing with
        | None ->
          (fun args -> call (arg :: args))
        | Some named ->
          (fun args ->
             let unboxed_arg = Variable.create "unboxed" in
             Flambda.Expr.create_let unboxed_arg
               (kind_of_repr arg_repr) (Prim (Unary (named, arg), dbg))
               (call (Simple.var unboxed_arg :: args))))
        (Env.find_simples env args)
        prim.prim_native_repr_args
        call []
    in
    let cont, handler_param =
      let unboxing : P.unary_primitive option =
        match prim.prim_native_repr_res with
        | Same_as_ocaml_repr -> None
        | Unboxed_float ->
          Some (P.Box_number Naked_float)
        | Unboxed_integer Pnativeint ->
          Some (P.Box_number Naked_nativeint)
        | Unboxed_integer Pint32 ->
          Some (P.Box_number Naked_int32)
        | Unboxed_integer Pint64 ->
          Some (P.Box_number Naked_int64)
        | Untagged_int ->
          Some (P.Num_conv { src = Naked_nativeint; dst = Tagged_immediate })
      in
      match unboxing with
      | None ->
        let body_env, handler_param = Env.add_var_like env id in
        let body = close t body_env body in
        body, handler_param
      | Some unboxing ->
        let handler_param = Variable.create (prim.prim_name ^ "_return") in
        let body_env, boxed_var = Env.add_var_like env id in
        let body = close t body_env body in
        Flambda.Expr.create_let boxed_var
          (K.value ())
          (Prim (Unary (unboxing, Simple.var handler_param), dbg)) body,
          handler_param
    in
    let handler : Flambda.Continuation_handler.t =
      let param =
        Typed_parameter.create (Parameter.wrap handler_param)
          (T.unknown return_kind)
      in
      { params = [param];
        stub = false;
        is_exn_handler = false;
        handler = cont;
      }
    in
    Let_cont {
      body = call;
      handlers = Non_recursive { name = continuation; handler; };
    };

  | Let (id, defining_expr, body) ->
    let body_env, var = Env.add_var_like env id in
    let cont (defining_expr : Flambda.Named.t) =
      (* CR pchambart: Not tail ! *)
      let body = close t body_env body in
      (* CR pchambart: Kind annotation on let should to go through Ilambda
         mshinwell: I added the following basic inference *)
      let kind =
        match defining_expr with
        | Simple (Name (Symbol _)) ->
          K.value ()
        | Simple (Const (Untagged_immediate _)) ->
          K.naked_immediate ()
        | Simple (Const (Tagged_immediate _)) ->
          K.value ()
        | Simple (Const (Naked_float _)) ->
          K.naked_float ()
        | Simple (Const (Naked_int32 _)) ->
          K.naked_int32 ()
        | Simple (Const (Naked_int64 _)) ->
          K.naked_int64 ()
        | Simple (Const (Naked_nativeint _)) ->
          K.naked_nativeint ()
        | Simple (Discriminant _)
        | Set_of_closures _ ->
          K.fabricated ()
        | Prim (prim, _dbg) ->
          begin match Flambda_primitive.result_kind prim with
          | Singleton kind -> kind
          | Unit -> K.unit ()
          | Never_returns -> K.value ()
          end
        | Simple (Name (Var _)) -> K.value ()
      in
      Flambda.Expr.create_let var kind defining_expr body
    in
    close_named t env defining_expr cont
  | Let_mutable _ ->
    Misc.fatal_error "[Let_mutable] found in closure conversion"
  | Let_rec (defs, body) ->
    let env =
      List.fold_right (fun (id,  _) env ->
          let env, _var = Env.add_var_like env id in
          env)
        defs env
    in
    let function_declarations =
      (* Functions will be named after the corresponding identifier in the
         [let rec]. *)
      List.map (function
          | (let_rec_ident,
              ({ kind; continuation_param; exn_continuation_param;
                 params; body; attr; loc; stub;
                 free_idents_of_body; } : Ilambda.function_declaration)) ->
            let closure_bound_var =
              Closure_id.wrap
                (Variable.create_with_same_name_as_ident let_rec_ident)
            in
            let function_declaration =
              Function_decl.create ~let_rec_ident:(Some let_rec_ident)
                ~closure_bound_var ~kind ~params ~continuation_param
                ~exn_continuation_param ~body
                ~attr ~loc ~free_idents_of_body ~stub
            in
            function_declaration)
        (* CR lmaurer: Do this in a better place? *)
        (List.map (fun (id, def) -> (id, remove_mutable_variables def)) defs)
    in
    (* We eliminate the [let rec] construction, instead producing a normal
       [Let] that binds a set of closures containing all of the functions.
       ([let rec] on non-functions was removed in [Prepare_lambda].)
    *)
    let name =
      (* The Microsoft assembler has a 247-character limit on symbol
         names, so we keep them shorter to try not to hit this. *)
      (* CR-soon mshinwell: We should work out how to shorten symbol names
         anyway, to help avoid enormous ELF string tables. *)
      if Sys.win32 then begin
        match defs with
        | (id, _)::_ -> (Ident.unique_name id) ^ "_let_rec"
        | _ -> "let_rec"
      end else begin
        String.concat "_and_"
          (List.map (fun (id, _) -> Ident.unique_name id) defs)
      end
    in
    let set_of_closures_var = Variable.create name in
    let set_of_closures =
      close_functions t env (Function_decls.create function_declarations)
    in
    let body =
      List.fold_left (fun body decl ->
          let let_rec_ident = Function_decl.let_rec_ident decl in
          let closure_bound_var = Function_decl.closure_bound_var decl in
          let let_bound_var = Env.find_var env let_rec_ident in
          (* Inside the body of the [let], each function is referred to by
             a [Project_closure] expression, which projects from the set of
             closures. *)
          (Flambda.Expr.create_let let_bound_var
             (K.value ())
             (Prim (Unary (Project_closure closure_bound_var,
                           Simple.var set_of_closures_var),
                    Debuginfo.none))
            body))
        (close t env body) function_declarations
    in
    Flambda.Expr.create_let set_of_closures_var (K.fabricated ())
      set_of_closures body
  | Let_cont let_cont ->
    if let_cont.is_exn_handler then begin
      assert (not let_cont.administrative);
      assert (List.length let_cont.params = 1);
      assert (let_cont.recursive = Asttypes.Nonrecursive);
    end;
    (* Inline out administrative redexes. *)
    if let_cont.administrative then begin
      assert (let_cont.recursive = Asttypes.Nonrecursive);
      let body_env =
        Env.add_administrative_redex env let_cont.name ~params:let_cont.params
          ~handler:let_cont.handler
      in
      close t body_env let_cont.body
    end else begin
      let handler_env, params = Env.add_vars_like env let_cont.params in
      let params =
        List.map (fun param ->
          Flambda.Typed_parameter.create (Parameter.wrap param)
            (T.unknown (K.value ())))
          params
      in
      let handler : Flambda.Continuation_handler.t =
        { params;
          stub = false;
          is_exn_handler = let_cont.is_exn_handler;
          handler = close t handler_env let_cont.handler;
        };
      in
      let handlers : Flambda.Let_cont_handlers.t =
        match let_cont.recursive with
        | Nonrecursive -> Non_recursive { name = let_cont.name; handler; }
        | Recursive ->
          Recursive (Continuation.Map.add let_cont.name handler
            Continuation.Map.empty)
      in
      Let_cont {
        body = close t env let_cont.body;
        handlers;
      };
    end
  | Apply { kind; func; args; continuation; exn_continuation;
      loc; should_be_tailcall = _; inlined; specialised; } ->
    let call_kind : Flambda.Call_kind.t =
      match kind with
      | Function -> Function Indirect_unknown_arity
      | Method { kind; obj; } ->
        let kind : Flambda.Call_kind.method_kind =
          match kind with
          | Self -> Self
          | Public -> Public
          | Cached -> Cached
        in
        Method {
          kind;
          obj = Env.find_name env obj;
        }
    in
    Apply ({
      call_kind;
      func = Env.find_name env func;
      args = Env.find_simples env args;
      continuation;
      exn_continuation;
      dbg = Debuginfo.from_location loc;
      inline = convert_inline_attribute_from_lambda inlined;
      specialise = convert_specialise_attribute_from_lambda specialised;
    })
  | Apply_cont (cont, trap_action, args) ->
    let args = Env.find_vars env args in
    begin match Env.find_administrative_redex env cont with
    | Some (params, handler) when trap_action = None ->
      let handler_env = Env.add_vars env params args in
      close t handler_env handler
    | _ ->
      let trap_action =
        Misc.Stdlib.Option.map (fun (trap_action : Ilambda.trap_action)
                  : Flambda.Trap_action.t ->
            match trap_action with
            | Push { id; exn_handler; } -> Push { id; exn_handler; }
            | Pop { id; exn_handler; } ->
              Pop { id; exn_handler; take_backtrace = false; })
          trap_action
      in
      Apply_cont (cont, trap_action, List.map Simple.var args)
    end
  | Switch (scrutinee, sw) ->
    (* CR mshinwell: work out how to share code with case above *)
    let arms =
      List.map (fun (case, arm) -> Discriminant.of_int_exn case, arm)
        sw.consts
    in
    let arms =
      match sw.failaction with
      | None ->
        Discriminant.Map.of_list arms
      | Some default ->
        Numbers.Int.Set.fold (fun case cases ->
          let case = Discriminant.of_int_exn case in
          if Discriminant.Map.mem case cases then
            cases
          else
            Discriminant.Map.add case default cases)
          (Numbers.Int.zero_to_n (sw.numconsts - 1))
          (Discriminant.Map.of_list arms)
    in
    Flambda.Expr.create_switch ~scrutinee:(Env.find_name env scrutinee) ~arms
  | Event (ilam, _) -> close t env ilam

and close_named t env (named : Ilambda.named)
      (cont : Flambda.Named.t -> Flambda.Expr.t) : Flambda.Expr.t =
  match named with
  | Var id ->
    let simple =
      if Ident.is_predef_exn id then begin
        let symbol = t.symbol_for_global' id in
        t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
        Simple.symbol symbol
      end else begin
        Simple.var (Env.find_var env id)
      end
    in
    cont (Simple simple)
  | Const cst ->
    cont (fst (close_const t cst))
  | Prim { prim = Pread_mutable _; args = _ } ->
    (* All occurrences of mutable variables bound by [Let_mutable] are
       identified by [Prim (Pread_mutable, ...)] in Ilambda. *)
    Misc.fatal_error "Pread_mutable found in closure conversion"
  | Prim { prim = Pgetglobal id; args = [] } when Ident.is_predef_exn id ->
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    cont (Simple (Simple.symbol symbol))
  | Prim { prim = Pgetglobal id; args = [] } ->
    assert (not (Ident.same id t.current_unit_id));
    let symbol = t.symbol_for_global' id in
    t.imported_symbols <- Symbol.Set.add symbol t.imported_symbols;
    cont (Simple (Simple.symbol symbol))
  | Prim { prim; args; loc; exception_continuation } ->
    Lambda_to_flambda_primitives.convert_and_bind prim
      ~args:(Env.find_simples env args)
      ~exception_continuation
      (Debuginfo.from_location loc) cont
  | Assign _ ->
    Misc.fatal_error "Assign found in closure conversion"

(** Perform closure conversion on a set of function declarations, returning a
    set of closures.  (The set will often only contain a single function;
    the only case where it cannot is for "let rec".) *)
and close_functions t external_env function_declarations : Flambda.Named.t =
  let all_free_idents =
    (* Filter out predefined exception identifiers, since they will be
       turned into symbols when we closure-convert the body. *)
    Ident.Set.filter (fun ident ->
        not (Ident.is_predef_exn ident))
      (Function_decls.all_free_idents function_declarations)
  in
  let var_within_closure_from_ident =
    Ident.Set.fold (fun id map ->
      let v = Variable.create_with_same_name_as_ident id in
      Ident.Map.add id (Var_within_closure.wrap v) map)
      all_free_idents Ident.Map.empty
  in
  let closure_id_from_ident =
    List.fold_left (fun map decl ->
      let id = Function_decl.let_rec_ident decl in
      let closure_id = Function_decl.closure_bound_var decl in
      Ident.Map.add id closure_id map)
      Ident.Map.empty
      (Function_decls.to_list function_declarations)
  in

  let close_one_function map decl =
    let body = Function_decl.body decl in
    let loc = Function_decl.loc decl in
    let dbg = Debuginfo.from_location loc in
    let params = Function_decl.params decl in
    let my_closure = Variable.create "my_closure" in

    let closure_bound_var =
      Function_decl.closure_bound_var decl
    in
    let unboxed_version =
      (* Better variable name *)
      Closure_id.wrap (Variable.create "unboxed_version")
    in
    let my_closure_id =
      match Function_decl.kind decl with
      | Curried -> closure_bound_var
      | Tupled -> unboxed_version
    in

    (* les variables libres sont:
       les paramètres: substitution directe en variables
       la fonction définie: accessible avec 'my_closure'
       les autres fonctions: accessibles avec un move
       let autres variables libres: accessibles avec une projection *)

    let var_within_closure_to_bind,
        var_for_ident_within_closure =
      Ident.Map.fold (fun id var_within_closure (to_bind, var_for_ident) ->
        let var = Variable.create_with_same_name_as_ident id in
        Variable.Map.add var var_within_closure to_bind,
        Ident.Map.add id var var_for_ident)
        var_within_closure_from_ident
        (Variable.Map.empty, Ident.Map.empty)
    in

    let project_closure_to_bind,
        var_for_project_closure =
      List.fold_left (fun (to_bind, var_for_ident) function_decl ->
        let let_rec_ident = Function_decl.let_rec_ident function_decl in
        let to_bind, var =
          if Ident.same (Function_decl.let_rec_ident function_decl)
               let_rec_ident then
            (* my_closure is already bound *)
            to_bind, my_closure
          else
            let variable =
              Variable.create_with_same_name_as_ident let_rec_ident
            in
            let closure_id =
              Ident.Map.find let_rec_ident closure_id_from_ident
            in
            Variable.Map.add variable closure_id to_bind, variable
        in
        to_bind,
        Ident.Map.add let_rec_ident var var_for_ident)
        (Variable.Map.empty, Ident.Map.empty)
        (Function_decls.to_list function_declarations)
    in

    let closure_env_without_parameters =
      let empty_env = Env.clear_local_bindings external_env in
      Env.add_var_map
        (Env.add_var_map empty_env var_for_ident_within_closure)
        var_for_project_closure
    in

    (* Create fresh variables for the elements of the closure (cf.
       the comment on [Function_decl.closure_env_without_parameters], above).
       This induces a renaming on [Function_decl.free_idents]; the results of
       that renaming are stored in [free_variables]. *)
    let closure_env =
      List.fold_right (fun (id, _) env ->
          let env, _var = Env.add_var_like env id in
          env)
        params closure_env_without_parameters
    in
    (* If the function is the wrapper for a function with an optional
       argument with a default value, make sure it always gets inlined.
       CR-someday pchambart: eta-expansion wrapper for a primitive are
       not marked as stub but certainly should *)
    let stub = Function_decl.stub decl in
    let param_vars =
      List.map (fun (p, t) -> Env.find_var closure_env p, t) params
    in
    let params =
      List.map (fun (p, t) ->
        Flambda.Typed_parameter.create (Parameter.wrap p)
          (flambda_type_of_lambda_value_kind t))
        param_vars
    in
    let body = close t closure_env body in
    let free_var_of_body =
      Name.set_to_var_set (Name_occurrences.in_terms (
        Flambda.Expr.free_names body))
    in
    let body =
      Variable.Map.fold (fun var closure_id body ->
        if Variable.Set.mem var free_var_of_body then
          let move =
            Flambda_primitive.Move_within_set_of_closures {
              move_from = my_closure_id;
              move_to = closure_id;
            }
          in
          Flambda.Expr.create_let var (K.value ())
            (Prim (Unary (move, Simple.var my_closure), Debuginfo.none))
            body
        else
          body
      ) project_closure_to_bind body
    in

    let body =
      Variable.Map.fold (fun var var_within_closure body ->
        if Variable.Set.mem var free_var_of_body then
          let projection =
            Flambda_primitive.Project_var
              (my_closure_id, var_within_closure)
          in
          Flambda.Expr.create_let var
            (K.value ())
            (Prim
               (Unary (projection, Simple.var my_closure),
                Debuginfo.none))
            body
        else
          body
      ) var_within_closure_to_bind body
    in

    let fun_decl =
      let closure_origin =
        Closure_origin.create my_closure_id
        (* Closure_origin.create (Closure_id.wrap unboxed_version) *)
      in
      let inline =
        convert_inline_attribute_from_lambda (Function_decl.inline decl)
      in
      let specialise =
        convert_specialise_attribute_from_lambda
          (Function_decl.specialise decl)
      in
      Flambda.Function_declaration.create
        ~my_closure
        ~params
        ~continuation_param:(Function_decl.continuation_param decl)
        ~exn_continuation_param:(Function_decl.exn_continuation_param decl)
        ~return_arity:[K.value ()]
        ~body ~stub ~dbg ~inline
        ~specialise
        ~is_a_functor:(Function_decl.is_a_functor decl)
        ~closure_origin
    in
    match Function_decl.kind decl with
    | Curried -> Closure_id.Map.add my_closure_id fun_decl map
    | Tupled ->
      let generic_function_stub =
        tupled_function_call_stub param_vars unboxed_version ~closure_bound_var
      in
      Closure_id.Map.add unboxed_version fun_decl
        (Closure_id.Map.add closure_bound_var generic_function_stub map)
  in
  let function_decls =
    Flambda.Function_declarations.create
      ~funs:
        (List.fold_left close_one_function Closure_id.Map.empty
          (Function_decls.to_list function_declarations))
  in
  (* The closed representation of a set of functions is a "set of closures".
     (For avoidance of doubt, the runtime representation of the *whole set* is
     a single block with tag [Closure_tag].) *)
  let set_of_closures =
    let in_closure =
      Ident.Map.fold (fun id var_within_closure map ->
        let external_var : Flambda.Free_var.t =
          { var = Env.find_var external_env id;
            (* CR pchambart: Should we populate that with a projection primitive ? *)
            equalities = Flambda_primitive.With_fixed_value.Set.empty;
          }
        in
        Var_within_closure.Map.add var_within_closure external_var map)
        var_within_closure_from_ident
        Var_within_closure.Map.empty
    in
    Flambda.Set_of_closures.create ~function_decls ~in_closure
      ~direct_call_surrogates:Closure_id.Map.empty
  in
  Set_of_closures set_of_closures

let ilambda_to_flambda ~backend ~module_ident ~size ~filename
      (ilam : Ilambda.program): Flambda_static.Program.t =
  let module Backend = (val backend : Backend_intf.S) in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let t =
    { current_unit_id = Compilation_unit.get_persistent_ident compilation_unit;
      symbol_for_global' = Backend.symbol_for_global';
      filename;
      imported_symbols = Symbol.Set.empty;
      declared_symbols = [];
    }
  in
  let module_symbol = Backend.symbol_for_global' module_ident in
  (* let block_symbol = *)
  (*   let linkage_name = Linkage_name.create "module_as_block" in *)
  (*   Symbol.create compilation_unit linkage_name *)
  (* in *)
  (* The global module block is built by accessing the fields of all the
     introduced symbols. *)
  (* CR-soon mshinwell for mshinwell: Add a comment describing how modules are
     compiled. *)
  (* let continuation = Continuation.create () in *)
  (* let main_module_block_expr = *)
  (*   let field_vars = *)
  (*     List.init size *)
  (*       (fun pos -> *)
  (*          let pos_str = string_of_int pos in *)
  (*          pos, Variable.create ("block_symbol_" ^ pos_str)) *)
  (*   in *)
  (*   let call_continuation : Flambda.Expr.t = *)
  (*     Apply_cont (continuation, None, List.map snd field_vars) *)
  (*   in *)
  (*   let block_symbol_var = Variable.create "block_symbol" in *)
  (*   let body = *)
  (*     List.fold_left (fun body (pos, var) -> *)
  (*       Flambda.Expr.create_let var *)
  (*         (K.value Must_scan) *)
  (*         (Prim (Pfield pos, [block_symbol_var], Debuginfo.none)) *)
  (*         body) *)
  (*       call_continuation field_vars *)
  (*   in *)
  (*   Flambda.Expr.create_let block_symbol_var *)
  (*     (K.value Must_scan) *)
  (*     (Read_symbol_field { symbol = block_symbol; logical_field = 0 }) *)
  (*     body *)
  (* in *)

  let block_var = Variable.create "module_block" in
  let assign_continuation = Continuation.create () in
  let field_vars =
    List.init size
      (fun pos ->
         let pos_str = string_of_int pos in
         Variable.create ("block_field_" ^ pos_str),
         K.value ())
  in
  let assign_continuation_body =
    let field_vars =
      List.init size
        (fun pos ->
           let pos_str = string_of_int pos in
           pos, Variable.create ("block_field_" ^ pos_str))
    in
    let body : Flambda.Expr.t =
      Apply_cont
        (assign_continuation, None,
         List.map (fun (_, var) -> Simple.var var) field_vars)
    in
    List.fold_left (fun body (pos, var) ->
      let pos = Immediate.int (Targetint.OCaml.of_int pos) in
      Flambda.Expr.create_let var (K.value ())
        (Prim (Binary (Block_load (Block (Value Unknown), Immutable),
                       Simple.var block_var,
                       Simple.const (Tagged_immediate pos)),
               Debuginfo.none))
        body)
      body field_vars
  in
  let assign_cont_def : Flambda.Continuation_handler.t =
    { params =
        [Flambda.Typed_parameter.create
           (Parameter.wrap block_var)
           (T.unknown (K.value ()))];
      stub = true;
      is_exn_handler = false;
      handler = assign_continuation_body;
    }
  in
  let expr : Flambda.Expr.t =
    Let_cont
      { handlers =
          Non_recursive { name = ilam.return_continuation;
                          handler = assign_cont_def };
        body = close t Env.empty ilam.expr; }
  in

  let computation : Program_body.computation =
    { expr;
      return_cont = assign_continuation;
      exception_cont = ilam.exception_continuation;
      computed_values = field_vars;
    }
  in
  let static_part : Static_part.t =
    Block (Tag.Scannable.zero, Immutable,
           List.map (fun (var, _) : Flambda_static0.Of_kind_value.t ->
             Dynamically_computed var)
             field_vars)
  in
  let program_body : Program_body.t =
    Define_symbol
      ({ computation = Some computation;
         static_structure =
           [module_symbol, K.value (), static_part]; },
       (Root module_symbol))
  in
  let program_body =
    (* CR mshinwell: Share with [Simplify_program] *)
    List.fold_left (fun program_body (symbol, static_part) : Program_body.t ->
        let static_structure =
          [symbol, K.value (), static_part]
        in
        let definition : Program_body.definition =
          { computation = None;
            static_structure;
          }
        in
        Define_symbol (definition, program_body))
      program_body
      t.declared_symbols
  in
  let imported_symbols =
    Symbol.Set.fold (fun symbol imported_symbols ->
        Symbol.Map.add symbol (K.value ()) imported_symbols)
      t.imported_symbols
      Symbol.Map.empty
  in
(* let module_initialize : Program_body.Initialize_symbol.t = *)
  (*   { expr = main_module_block_expr; *)
  (*     return_cont = continuation; *)
  (*     return_arity = List.init size (fun _ -> K.value Must_scan); *)
  (*   } *)
  (* in *)
  (* let module_initializer : Program_body.t = *)
  (*   Initialize_symbol ( *)
  (*     block_symbol, *)
  (*     block_initialize, *)
  (*     Initialize_symbol ( *)
  (*       module_symbol, *)
  (*       module_initialize, *)
  (*       End module_symbol)) *)
  (* in *)
  (* let program_body = *)
  (*   List.fold_left *)
  (*     (fun program_body (symbol, constant) : Program_body.t -> *)
  (*        Let_symbol (symbol, constant, program_body)) *)
  (*     module_initializer *)
  (*     t.declared_symbols *)
  (* in *)
  { imported_symbols;
    body = program_body;
  }


(* CR mshinwell: read carefully.  Moved here from Flambda_type

  let refine_using_value_kind t (kind : Lambda.value_kind) =
    match kind with
    | Pgenval -> t
    | Pfloatval ->
      begin match t.descr with
      | Boxed_or_encoded_number (Boxed Float,
          { descr = Naked_number (Float _); _ }) ->
        t
      | Unknown ((Unboxed_float | Bottom), reason) ->
        { t with
          descr = Boxed_or_encoded_number (Boxed Float,
            just_descr (Unknown (K.unboxed_float (), reason)));
        }
      | Unknown (
          (Value | Tagged_int | Naked_int | Naked_int32 | Naked_int64
            | Unboxed_nativeint), _) ->
        Misc.fatal_errorf "Wrong type for Pfloatval kind: %a"
          print t
      | Union _
      | Naked_number _
      | Boxed_or_encoded_number _
      | Set_of_closures _
      | Closure _
      | Immutable_string _
      | Mutable_string _
      | Float_array _
      | Bottom ->
        (* Invalid _ *)
        { t with descr = Bottom }
      | Load_lazily _ ->
        (* We don't know yet *)
        t
      end
    (* CR mshinwell: Do we need more cases here?  We could add Pintval *)
    | _ -> t
*)
