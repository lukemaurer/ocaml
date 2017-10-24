(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2017 OCamlPro SAS                                    *)
(*   Copyright 2014--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-9-30-40-41-42"]

module F = Flambda

module Static_part = Flambda_static0.Static_part

module Program_body = struct
  include Flambda_static0.Program_body

  module Iterators = struct
    let iter_toplevel_exprs_in_definition defn ~f =
      begin match defn.computation with
      | None -> ()
      | Some computation ->
        let continuation_arity =
          List.map (fun (_var, kind) -> kind) computation.computed_values
        in
        f ~continuation_arity computation.return_cont computation.expr
      end;
      List.iter (fun (_sym, (static_part : Static_part.t) ->
          | Set_of_closures set ->
            Flambda.Set_of_closures.Iterators.iter_function_bodies set ~f
          | Block _
          | Closure _
          | Boxed_float _
          | Boxed_int32 _
          | Boxed_int64 _
          | Boxed_nativeint _
          | Mutable_float_array _
          | Immutable_float_array _
          | Mutable_string _
          | Immutable_string _ -> ())
        defn.static_structure

    let iter_toplevel_exprs (t : t) ~f =
      match t with
      | Define_symbol (defn, t)
      | Define_symbol_rec (defn, t) ->
        iter_toplevel_exprs_in_definition defn ~f;
        iter_toplevel_exprs t ~f
      | Root _ -> ()

(*
      Toplevel_only.iter_exprs program
        ~f:(fun ~continuation_arity cont expr ->
          let rec iter_expr ~continuation_arity cont expr =
            Flambda.Expr.Iterators.iter_named (fun (named : Flambda.Named.t) ->
                match named with
                | Set_of_closures set_of_closures ->
                  Closure_id.Map.iter
                    (fun _ (function_decl : F.Function_declaration.t) ->
                      iter_expr ~continuation_arity:function_decl.return_arity
                        function_decl.continuation_param
                        function_decl.body)
                    set_of_closures.function_decls.funs
                | _ -> ())
              expr;
            f ~continuation_arity cont expr
          in
          iter_expr ~continuation_arity cont expr)
*)
  end
end

module Program = struct
  include Flambda_static0.Program

  let imported_symbols t = t.imported_symbols

  let root_symbol t =
    let rec loop (body : Program_body.t) =
      match body with
      | Define_symbol (_, body)
      | Define_symbol_rec (_, body)
      | Root root -> root
    in
    loop t.body

  module Iterators = struct
(*
    let iter_set_of_closures (program : t) ~f =
      let rec loop (program : Program_body.t) =
        match program with
        | Let_symbol (_, Set_of_closures set_of_closures, program) ->
          f ~constant:true set_of_closures;
          Closure_id.Map.iter
            (fun _ (function_decl : F.Function_declaration.t) ->
              F.Expr.Iterators.iter_sets_of_closures (f ~constant:false)
                function_decl.body)
            set_of_closures.function_decls.funs;
          loop program
        | Let_rec_symbol (defs, program) ->
          List.iter (function
              | (_, CDV.Set_of_closures set_of_closures) ->
                f ~constant:true set_of_closures;
                Closure_id.Map.iter
                  (fun _ (function_decl : F.Function_declaration.t) ->
                    F.Expr.Iterators.iter_sets_of_closures (f ~constant:false)
                      function_decl.body)
                  set_of_closures.function_decls.funs
              | _ -> ()) defs;
          loop program
        | Let_symbol (_, _, program) ->
          loop program
        | Initialize_symbol (_, descr, program) ->
          Flambda.Expr.Iterators.iter_sets_of_closures (f ~constant:false)
            descr.expr;
          loop program
        | Effect (expr, _cont, program) ->
          Flambda.Expr.Iterators.iter_sets_of_closures (f ~constant:false) expr;
          loop program
        | End _ -> ()
      in
      loop program.program_body

    let iter_constant_defining_values (program : t) ~f =
      let rec loop (program : Program_body.t) =
        match program with
        | Let_symbol (_, const, program) ->
          f const;
          loop program
        | Let_rec_symbol (defs, program) ->
          List.iter (fun (_, const) -> f const) defs;
          loop program
        | Initialize_symbol (_, _, program) ->
          loop program
        | Effect (_, _, program) ->
          loop program
        | End _ -> ()
      in
      loop program.program_body

    module Toplevel_only = struct
      let iter_exprs (program : t) ~f =
        let rec loop (program : Program_body.t) =
          match program with
          | Let_symbol (_, Set_of_closures set_of_closures, program) ->
            Closure_id.Map.iter
              (fun _ (function_decl : F.Function_declaration.t) ->
                f ~continuation_arity:function_decl.return_arity
                  function_decl.continuation_param function_decl.body)
              set_of_closures.function_decls.funs;
            loop program
          | Let_rec_symbol (defs, program) ->
            List.iter (function
                | (_, CDV.Set_of_closures set_of_closures) ->
                  Closure_id.Map.iter
                    (fun _ (function_decl : F.Function_declaration.t) ->
                      f ~continuation_arity:function_decl.return_arity
                        function_decl.continuation_param function_decl.body)
                    set_of_closures.function_decls.funs
                | _ -> ()) defs;
            loop program
          | Let_symbol (_, _, program) ->
            loop program
          | Initialize_symbol (_, descr, program) ->
            f ~continuation_arity:descr.return_arity descr.return_cont
              descr.expr;
            loop program
          | Effect (expr, cont, program) ->
            f ~continuation_arity:[] cont expr;
            loop program
          | End _ -> ()
        in
        loop program.program_body
    end 
*)

    let iter_toplevel_exprs (t : t) ~f =
      Program_body.Iterators.iter_toplevel_exprs t.body ~f

    let iter_apply t ~f =
      iter_toplevel_exprs t
        ~f:(fun ~continuation_arity:_ _cont expr ->
          Flambda.Expr.Iterators.iter (function
              | Apply apply -> f apply
              | _ -> ())
            (fun _ -> ())
            expr)

    let iter_named t ~f =
      iter_toplevel_exprs t ~f:(fun ~continuation_arity:_ _ e ->
        Flambda.Expr.Iterators.iter_named f e)
  end
(*
  module Mappers = struct
    let map_sets_of_closures (program : t)
          ~(f : F.Set_of_closures.t -> F.Set_of_closures.t) =
      let rec loop (program : Program_body.t)
            : Program_body.t =
        let map_constant_set_of_closures (set_of_closures:F.Set_of_closures.t) =
          let done_something = ref false in
          let function_decls =
            let funs =
              Closure_id.Map.map
                (fun (function_decl : F.Function_declaration.t) ->
                  let body =
                    Flambda.Expr.Mappers.map_sets_of_closures
                      function_decl.body ~f
                  in
                  if body == function_decl.body then
                    function_decl
                  else begin
                    done_something := true;
                    F.Function_declaration.update_body function_decl ~body
                  end)
                set_of_closures.function_decls.funs
            in
            if not !done_something then
              set_of_closures.function_decls
            else
              F.Function_declarations.update set_of_closures.function_decls
                ~funs
          in
          let new_set_of_closures = f set_of_closures in
          if new_set_of_closures == set_of_closures then
            set_of_closures
          else
            F.Set_of_closures.create ~function_decls
              ~free_vars:set_of_closures.free_vars
              ~direct_call_surrogates:set_of_closures.direct_call_surrogates
        in
        match program with
        | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
          let new_set_of_closures =
            map_constant_set_of_closures set_of_closures
          in
          let new_program' = loop program' in
          if new_set_of_closures == set_of_closures
              && new_program' == program' then
            program
          else
            let const = CDV.create_set_of_closures new_set_of_closures in
            Let_symbol (symbol, const, new_program')
        | Let_symbol (symbol, const, program') ->
          let new_program' = loop program' in
          if new_program' == program' then
            program
          else
            Let_symbol (symbol, const, new_program')
        | Let_rec_symbol (defs, program') ->
          let done_something = ref false in
          let defs =
            List.map (function
                | (var, CDV.Set_of_closures set_of_closures) ->
                  let new_set_of_closures =
                    map_constant_set_of_closures set_of_closures
                  in
                  if not (new_set_of_closures == set_of_closures) then begin
                    done_something := true
                  end;
                  let const = CDV.create_set_of_closures new_set_of_closures in
                  var, const
                | def -> def)
              defs
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Let_rec_symbol (defs, loop program')
        | Initialize_symbol (symbol, descr, program') ->
          let done_something = ref false in
          let descr : Program_body.Initialize_symbol.t =
            let new_expr =
              Flambda.Expr.Mappers.map_sets_of_closures descr.expr ~f
            in
            if not (new_expr == descr.expr) then begin
              done_something := true
            end;
            { descr with
              expr = new_expr;
            }
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Initialize_symbol (symbol, descr, new_program')
        | Effect (expr, cont, program') ->
          let new_expr =
            Flambda.Expr.Mappers.map_sets_of_closures expr ~f
          in
          let new_program' = loop program' in
          if new_expr == expr && new_program' == program' then
            program
          else
            Effect (new_expr, cont, new_program')
        | End _ -> program
      in
      { program with
        program_body = loop program.program_body;
      }

    let map_toplevel_exprs (program : t) ~(f : F.Expr.t -> F.Expr.t) =
      let rec loop (program : Program_body.t) : Program_body.t =
        let map_constant_set_of_closures
              (set_of_closures : F.Set_of_closures.t) =
          let done_something = ref false in
          let funs =
            Closure_id.Map.map
              (fun (function_decl : F.Function_declaration.t) ->
                let body = f function_decl.body in
                if body == function_decl.body then
                  function_decl
                else begin
                  done_something := true;
                  F.Function_declaration.update_body function_decl ~body
                end)
              set_of_closures.function_decls.funs
          in
          if not !done_something then
            set_of_closures
          else
            let function_decls =
              F.Function_declarations.update set_of_closures.function_decls
                ~funs
            in
            F.Set_of_closures.create ~function_decls
              ~free_vars:set_of_closures.free_vars
              ~direct_call_surrogates:set_of_closures.direct_call_surrogates
        in
        (* CR-soon mshinwell: code very similar to the above function *)
        match program with
        | Let_symbol (symbol, Set_of_closures set_of_closures, program') ->
          let new_set_of_closures =
            map_constant_set_of_closures set_of_closures
          in
          let new_program' = loop program' in
          if new_set_of_closures == set_of_closures
              && new_program' == program' then
            program
          else
            let const = CDV.create_set_of_closures new_set_of_closures in
            Let_symbol (symbol, const, new_program')
        | Let_symbol (symbol, const, program') ->
          let new_program' = loop program' in
          if new_program' == program' then
            program
          else
            Let_symbol (symbol, const, new_program')
        | Let_rec_symbol (defs, program') ->
          let done_something = ref false in
          let defs =
            List.map (function
                | (var, CDV.Set_of_closures set_of_closures) ->
                  let new_set_of_closures =
                    map_constant_set_of_closures set_of_closures
                  in
                  if not (new_set_of_closures == set_of_closures) then begin
                    done_something := true
                  end;
                  let const = CDV.create_set_of_closures new_set_of_closures in
                  var, const
                | def -> def)
              defs
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Let_rec_symbol (defs, new_program')
        | Initialize_symbol (symbol, descr, program') ->
          let done_something = ref false in
          let descr : Program_body.Initialize_symbol.t =
            let new_expr = f descr.expr in
            if not (new_expr == descr.expr) then begin
              done_something := true
            end;
            { descr with
              expr = new_expr;
            }
          in
          let new_program' = loop program' in
          if new_program' == program' && not !done_something then
            program
          else
            Initialize_symbol (symbol, descr, new_program')
        | Effect (expr, cont, program') ->
          let new_expr = f expr in
          let new_program' = loop program' in
          if new_expr == expr && new_program' == program' then
            program
          else
            Effect (new_expr, cont, new_program')
        | End _ -> program
      in
      { program with
        program_body = loop program.program_body;
      }

    let map_named t ~(f : Variable.t -> F.Named.t -> F.Named.t) =
      map_toplevel_exprs t ~f:(fun expr ->
        Flambda.Expr.Mappers.map_named_with_id f expr)
  end

  let all_sets_of_closures program =
    let list = ref [] in
    Iterators.iter_set_of_closures program
      ~f:(fun ~constant:_ set_of_closures ->
          list := set_of_closures :: !list);
    !list

  let all_sets_of_closures_map program =
    let r = ref Set_of_closures_id.Map.empty in
    Iterators.iter_set_of_closures program
      ~f:(fun ~constant:_ set_of_closures ->
        r := Set_of_closures_id.Map.add
            set_of_closures.function_decls.set_of_closures_id
            set_of_closures !r);
    !r

  let all_function_decls_indexed_by_set_of_closures_id program =
    Set_of_closures_id.Map.map
      (fun { Flambda.Set_of_closures. function_decls; _ } -> function_decls)
      (all_sets_of_closures_map program)

  (* XXX there may be multiple ones with the same closure ID now *)
  let all_function_decls_indexed_by_closure_id _program = assert false
(*
    let aux_fun function_decls closure_id _ map =
      Closure_id.Map.add closure_id function_decls map
    in
    let aux _ ({ function_decls; _ } : Flambda.Set_of_closures.t) map =
      Closure_id.Map.fold (aux_fun function_decls) function_decls.funs map
    in
    Set_of_closures_id.Map.fold aux (all_sets_of_closures_map program)
      Closure_id.Map.empty
*)

  let all_lifted_constants (program : t) =
    let rec loop (program : Program_body.t) =
      match program with
      | Let_symbol (symbol, decl, program) -> (symbol, decl) :: (loop program)
      | Let_rec_symbol (decls, program) ->
        List.fold_left (fun l (symbol, decl) -> (symbol, decl) :: l)
          (loop program)
          decls
      | Initialize_symbol (_, _, program)
      | Effect (_, _, program) -> loop program
      | End _ -> []
    in
    loop program.program_body

  let all_lifted_constant_sets_of_closures program =
    let set = ref Set_of_closures_id.Set.empty in
    List.iter (function
        | (_, CDV.Set_of_closures {
            function_decls = { set_of_closures_id } }) ->
          set := Set_of_closures_id.Set.add set_of_closures_id !set
        | _ -> ())
      (all_lifted_constants program);
    !set

  let all_lifted_constants_as_map program =
    Symbol.Map.of_list (all_lifted_constants program)

  let needed_import_symbols (program : t) =
    let dependencies = free_symbols program in
    let defined_symbol =
      Symbol.Set.union
        (Symbol.Set.of_list
          (List.map fst (all_lifted_constants program)))
        (Symbol.Set.of_list
          (List.map (fun (s, _) -> s) (initialize_symbols program)))
    in
    Symbol.Set.diff dependencies defined_symbol

  let introduce_needed_import_symbols program : t =
    { program with
      imported_symbols = needed_import_symbols program;
    }

  let make_closure_map _program = assert false (* XXX same as above! *)
(*
    let map = ref Closure_id.Map.empty in
    let add_set_of_closures ~constant:_ : Flambda.Set_of_closures.t -> unit =
        fun { function_decls } ->
      CLosure_id.Map.iter (fun closure_id _ ->
          map := Closure_id.Map.add closure_id function_decls !map)
        function_decls.funs
    in
    Iterators.iter_set_of_closures program ~f:add_set_of_closures;
    !map
*)

*)
end


(*

(*
  let declare_simple t static_part =
    let symbol = Symbol.create "boxed_float" in
    let definition =
      { static_structure = [symbol, static_part];
        computation = None;
      }
    in
    let definition_group =
      { recursive = Nonrecursive;
        definitions = [definition];
      }
    in
    { t with
      definitions = definition_group :: t.definitions;
    }

  let declare_boxed_float t f = declare_simple t (Boxed_float (Const f))
  let declare_boxed_int32 t n = declare_simple t (Boxed_int32 (Const n))
  let declare_boxed_int64 t n = declare_simple t (Boxed_int64 (Const n))
  let declare_boxed_nativeint t n = declare_simple t (Boxed_nativeint (Const n))

  let declare_immutable_string t s =
    declare_simple t (Immutable_string (Const s))

  let declare_mutable_string t ~initial_value =
    declare_simple t (Immutable_string (Const { initial_value; }))

  let declare_float_array t fs =
    let fs = List.map (fun f : _ or_variable -> Const f) fs in
    declare_simple t (Immutable_float_array fs)

  let declare_block t tag fields =
    let fields = List.map (fun s : Field_of_kind_value.t -> Symbol s) fields in
    declare_simple t (Block (tag, fields))

  let declare_single_field_block_pointing_at t thing kind =
    let field : Field_of_kind_value.t = Dynamically_computed thing in
    declare_simple t (Block (Tag.Scannable.zero, [field]))
*)
*)
