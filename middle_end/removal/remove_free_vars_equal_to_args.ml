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

(* XXX this needs fixing for the specialised_args removal change *)

let pass_name = "remove-free-vars-equal-to-args"
let () = Pass_wrapper.register ~pass_name

(*

let rewrite_one_function_decl ~(function_decl : Flambda.Function_declaration.t)
      ~back_free_vars ~specialised_args =
  let params_for_equal_free_vars =
    List.fold_left (fun subst param ->
        match Variable.Map.find param specialised_args with
        | exception Not_found ->
          (* param is not specialised *)
          subst
        | (spec_to : Flambda.specialised_to) ->
          match spec_to.var with
          | Some outside_var ->
            begin match Variable.Map.find outside_var back_free_vars with
            | exception Not_found ->
              (* No free variables equal to the param *)
              subst
            | set ->
              (* Replace the free variables equal to a parameter *)
              Variable.Set.fold (fun free_var subst ->
                  Variable.Map.add free_var param subst)
                set subst
            end
          | None ->
            Misc.fatal_errorf "No equality to variable for specialised arg %a"
              Variable.print param)
      Variable.Map.empty (Parameter.List.vars function_decl.params)
  in
  if Variable.Map.is_empty params_for_equal_free_vars then
    function_decl
  else
    let body =
      Flambda.Expr.toplevel_substitution
        params_for_equal_free_vars
        function_decl.body
    in
    Flambda.Function_declaration.update_body function_decl ~body

*)

let rewrite_one_set_of_closures (set_of_closures : Flambda.Set_of_closures.t) =
  ignore set_of_closures;
  None

(*
  let back_free_vars =
    Variable.Map.fold (fun var (outside_var : Flambda.Free_var.t) map ->
        let set =
          match Variable.Map.find outside_var.var map with
          | exception Not_found -> Variable.Set.singleton var
          | set -> Variable.Set.add var set
        in
        Variable.Map.add outside_var.var set map)
      set_of_closures.free_vars Variable.Map.empty
  in
  let done_something = ref false in
  let funs =
    Variable.Map.map (fun function_decl ->
        let new_function_decl =
          rewrite_one_function_decl ~function_decl ~back_free_vars
            ~specialised_args:set_of_closures.specialised_args
        in
        if not (new_function_decl == function_decl) then begin
          done_something := true
        end;
        new_function_decl)
      set_of_closures.function_decls.funs
  in
  if not !done_something then
    None
  else
    let function_decls =
      Flambda.Function_declarations.update
        set_of_closures.function_decls ~funs
    in
    let set_of_closures =
      Flambda.Set_of_closures.create
        ~function_decls
        ~free_vars:set_of_closures.free_vars
        ~specialised_args:set_of_closures.specialised_args
        ~direct_call_surrogates:set_of_closures.direct_call_surrogates
    in
    Some set_of_closures
*)

let run set_of_closures =
  Pass_wrapper.with_dump ~pass_name ~input:set_of_closures
    ~print_input:Flambda.Set_of_closures.print
    ~print_output:Flambda.Set_of_closures.print
    ~f:(fun () -> rewrite_one_set_of_closures set_of_closures)