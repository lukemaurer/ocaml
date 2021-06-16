(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2019 OCamlPro SAS                                    *)
(*   Copyright 2014--2019 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-4-30-40-41-42"]

open! Flambda

module DA = Downwards_acc
module DE = Downwards_env
module K = Flambda_kind
module KP = Kinded_parameter
module P = Flambda_primitive
module T = Flambda_type
module TEE = T.Typing_env_extension
module UA = Upwards_acc
module UE = Upwards_env

type 'a after_rebuild =
     Rebuilt_expr.t
  -> Upwards_acc.t
  -> 'a

type 'a rebuild =
     Upwards_acc.t
  -> after_rebuild:'a after_rebuild
  -> 'a

type ('a, 'b) down_to_up =
     Downwards_acc.t
  -> rebuild:'a rebuild
  -> 'b

type 'a expr_simplifier =
     Downwards_acc.t
  -> 'a
  -> down_to_up:(Rebuilt_expr.t * Upwards_acc.t,
       Rebuilt_expr.t * Upwards_acc.t) down_to_up
  -> Rebuilt_expr.t * Upwards_acc.t

type simplify_toplevel =
     Downwards_acc.t
  -> Expr.t
  -> return_continuation:Continuation.t
  -> return_arity:Flambda_arity.With_subkinds.t
  -> Exn_continuation.t
  -> return_cont_scope:Scope.t
  -> exn_cont_scope:Scope.t
  -> Rebuilt_expr.t * Upwards_acc.t

let is_self_tail_call dacc apply =
  let denv = DA.denv dacc in
  match DE.closure_info denv with
  | Not_in_a_closure -> false
  | In_a_set_of_closures_but_not_yet_in_a_specific_closure ->
    (* It's safe to return false here (even, though this should
       not happen) *)
    false
  | Closure { code_id = fun_code_id;
              return_continuation = fun_cont;
              exn_continuation = fun_exn_cont; } ->
    (* 1st check: exn continuations match *)
    let apply_exn_cont = Apply.exn_continuation apply in
    Exn_continuation.equal fun_exn_cont apply_exn_cont &&
    (* 2nd check: return continuations match *)
    begin match Apply.continuation apply with
    (* a function that raises unconditionally can be a tail-call *)
    | Never_returns -> true
    | Return apply_cont -> Continuation.equal fun_cont apply_cont
    end &&
    (* 3rd check: check this is a self-call. *)
    begin match Apply.call_kind apply with
    | Function (Direct { code_id = apply_code_id; _ }) ->
      Code_id.equal fun_code_id apply_code_id
    | Method _ | C_call _
    | Function (Indirect_known_arity _ | Indirect_unknown_arity)
      -> false
    end

let simplify_projection dacc ~original_term ~deconstructing ~shape ~result_var
      ~result_kind =
  if !Clflags.dump_rawflambda then begin
    Format.eprintf
      "@[<hov 1>simplify_projection@ \
       @[<hov 1>(original_term@ %a)@]@ \
       @[<hov 1>(deconstructing@ %a)@]@ \
       @[<hov 1>(shape@ %a)@]@ \
       @[<hov 1>(result_var@ %a)@]@ \
       @[<hov 1>(result_kind@ %a)@]@]@.%!"
      Named.print original_term
      T.print deconstructing
      T.print shape
      Var_in_binding_pos.print result_var
      K.print result_kind
  end;
  T.with_tracing_meets begin fun () ->
  let env = DA.typing_env dacc in
  match T.meet_shape env deconstructing ~shape ~result_var ~result_kind with
  | Bottom -> Simplified_named.invalid (), TEE.empty (), dacc
  | Ok env_extension ->
    Simplified_named.reachable original_term, env_extension, dacc
end

let update_exn_continuation_extra_args uacc ~exn_cont_use_id apply =
  let exn_cont_rewrite =
    UE.find_apply_cont_rewrite (UA.uenv uacc)
      (Exn_continuation.exn_handler (Apply.exn_continuation apply))
  in
  match exn_cont_rewrite with
  | None -> apply
  | Some rewrite ->
    Apply.with_exn_continuation apply
      (Expr_builder.rewrite_exn_continuation rewrite exn_cont_use_id
        (Apply.exn_continuation apply))

(* generate the projection of the i-th field of a n-tuple *)
let project_tuple ~dbg ~size ~field tuple =
  let module BAK = P.Block_access_kind in
  let bak : BAK.t = Values {
    field_kind = Any_value;
    tag = Tag.Scannable.zero;
    size = Known (Targetint.OCaml.of_int size);
  } in
  let mutability : Mutability.t = Immutable in
  let index = Simple.const_int (Targetint.OCaml.of_int field) in
  let prim = P.Binary (Block_load (bak, mutability), tuple, index) in
  Named.create_prim prim dbg

let split_direct_over_application apply ~param_arity =
  let arity = List.length param_arity in
  let args = Apply.args apply in
  assert (arity < List.length args);
  let full_app_args, remaining_args = Misc.Stdlib.List.split_at arity args in
  let func_var = Variable.create "full_apply" in
  let perform_over_application =
    Apply.create ~callee:(Simple.var func_var)
      ~continuation:(Apply.continuation apply)
      (Apply.exn_continuation apply)
      ~args:remaining_args
      ~call_kind:(Call_kind.indirect_function_call_unknown_arity ())
      (Apply.dbg apply)
      ~inline:(Apply.inline apply)
      ~inlining_state:(Apply.inlining_state apply)
  in
  let after_full_application = Continuation.create () in
  let after_full_application_handler =
    let func_param = KP.create func_var K.With_subkind.any_value in
    let free_names_of_expr = Apply.free_names perform_over_application in
    Continuation_handler.create [func_param]
      ~handler:(Expr.create_apply perform_over_application)
      ~free_names_of_handler:(Known free_names_of_expr)
      ~is_exn_handler:false
  in
  let full_apply =
    Apply.with_continuation_callee_and_args apply
      (Return after_full_application)
      ~callee:(Apply.callee apply)
      ~args:full_app_args
  in
  let expr =
    Let_cont.create_non_recursive after_full_application
      after_full_application_handler
      ~body:(Expr.create_apply full_apply)
      ~free_names_of_body:(Known (Apply.free_names full_apply))
  in
  expr
