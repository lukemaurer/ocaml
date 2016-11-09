(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2016 OCamlPro SAS                                    *)
(*   Copyright 2014--2016 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

let should_copy (named:Flambda.named) =
  match named with
  | Symbol _ | Read_symbol_field _ | Const _ -> true
  | _ -> false

let rec lift (expr : Flambda.expr) ~copies =
  match expr with
  | Let_cont ({ name; body;
      handler = Handler {
          params = [param]; recursive = Nonrecursive; handler; };
      _ } as let_cont) ->
    let free_conts, lifted, body = lift body ~copies in
    let our_cont = Continuation.Set.singleton name in
    if Continuation.Set.equal free_conts our_cont then begin
      (* The body of this [Let_cont] can only return through [cont], which
         means that [handler] postdominates [body].  As such we can cut off
         [body] and put it inside an [Initialize_symbol] whose continuation
         is [handler]. *)
      let symbol = Flambda_utils.make_variable_symbol param in
Format.eprintf "PD condition satisfied for cont %a, param %a, sym %a\n%!"
  Continuation.print name
  Variable.print param
  Symbol.print symbol;
      let copies = (param, ((Symbol symbol) : Flambda.named))::copies in
      let free_conts, lifted', handler = lift handler ~copies in
      let lifted = (name, param, symbol, body, copies) :: lifted' @ lifted in
      let expr : Flambda.expr =
        Flambda.create_let param (Symbol symbol) handler
      in
      free_conts, lifted, expr
    end else begin
      let free_conts =
        Continuation.Set.union free_conts
          (Flambda_utils.free_continuations handler)
      in
      let expr : Flambda.expr = Let_cont { let_cont with body; } in
      free_conts, lifted, expr
    end
  | Let { var; defining_expr; body; _ } when should_copy defining_expr ->
    let copies = (var, defining_expr)::copies in
    let free_conts, lifted, body = lift body ~copies in
    let body = Flambda.create_let var defining_expr body in
    free_conts, lifted, body
  | Let { var; defining_expr; body; _ } ->
    let symbol = Flambda_utils.make_variable_symbol var in
    let free_conts, lifted, body = lift body ~copies in
    let cont = Continuation.create () in
    let expr : Flambda.expr =
      Flambda.create_let var defining_expr (Apply_cont (cont, [var]))
    in
    let lifted = (cont, var, symbol, expr, copies) :: lifted in
    let body = Flambda.create_let var (Symbol symbol) body in
    free_conts, lifted, body
  | Let_mutable ({ body; _ } as let_mutable) ->
    let free_conts, lifted, body = lift body ~copies in
    let body : Flambda.t = Let_mutable { let_mutable with body; } in
    free_conts, lifted, body
  | Let_cont _ | Apply _ | Apply_cont _ | Switch _ ->
    let free_conts = Flambda_utils.free_continuations expr in
    free_conts, [], expr

(* CR-someday mshinwell: Try to avoid having a separate substitution phase. *)
let introduce_symbols expr =
  let _free_conts, lifted_rev, expr = lift expr ~copies:[] in
  let used_variables = Flambda.used_variables expr in
  let lifted_rev =
    List.map (fun (cont, var, symbol, expr, copies) ->
        let copies, subst =
          List.fold_left (fun (copies, subst) (var, defining_expr) ->
              let var' = Variable.rename var in
              let copies = (var', defining_expr) :: copies in
              copies, Variable.Map.add var var' subst)
            ([], Variable.Map.empty)
            copies
        in
        let copies =
          List.map (fun (var, defining_expr) ->
              let defining_expr =
                Flambda_utils.toplevel_substitution_named subst defining_expr
              in
              var, defining_expr)
            copies
        in
        let expr = Flambda_utils.toplevel_substitution subst expr in
        cont, var, symbol, expr, copies)
      lifted_rev
  in
  (lifted_rev, used_variables), expr

let add_extracted (lifted_rev, used_variables) program_body =
  List.fold_left (fun acc (cont, var, symbol, expr, copies)
            : Flambda.program_body ->
      let expr =
        List.fold_left (fun expr (var, defining_expr) ->
            if Variable.Set.mem var (Flambda.free_variables expr) then
              Flambda.create_let var defining_expr expr
            else
              expr)
          expr
          copies
      in
      if Variable.Set.mem var used_variables then
        Initialize_symbol (symbol, Tag.zero, [expr, cont], acc)
      else
        Effect (expr, cont, acc))
    program_body
    lifted_rev

let rec split_program (program : Flambda.program_body) : Flambda.program_body =
  match program with
  | End s -> End s
  | Let_symbol (s, def, program) ->
    Let_symbol (s, def, split_program program)
  | Let_rec_symbol (defs, program) ->
    Let_rec_symbol (defs, split_program program)
  | Effect (expr, cont, program) ->
    let program = split_program program in
    let introduced, expr = introduce_symbols expr in
    add_extracted introduced (Flambda.Effect (expr, cont, program))
  | Initialize_symbol (symbol, tag, ((_::_::_) as fields), program) ->
    (* CR-someday pchambart: currently the only initialize_symbol with more
       than 1 field is the module block. This could evolve, in that case
       this pattern should be handled properly. *)
    Initialize_symbol (symbol, tag, fields, split_program program)
  | Initialize_symbol (sym, tag, [], program) ->
    Let_symbol (sym, Block (tag, []), split_program program)
  | Initialize_symbol (symbol, tag, [field, cont], program) ->
    let program = split_program program in
    let introduced, field = introduce_symbols field in
    add_extracted introduced
      (Flambda.Initialize_symbol (symbol, tag, [field, cont], program))

let lift ~backend:_ (program : Flambda.program) =
  { program with
    program_body = split_program program.program_body;
  }
