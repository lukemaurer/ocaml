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

(** See the Flambda manual chapter for an explanation in prose of the
    inlining decision procedure. *)

(** Try to inline a full application of a known function, guided by various
    heuristics. *)
val for_call_site
   : env:Simplify_env.t
  -> r:Simplify_result.t
  -> function_decls:Flambda.Function_declarations.t
  -> lhs_of_application:Variable.t
  -> closure_id_being_applied:Closure_id.t
  -> function_decl:Flambda.Function_declaration.t
  -> value_set_of_closures:Flambda_type.set_of_closures
  -> args:Variable.t list
  -> args_approxs:Flambda_type.t list
  -> continuation:Continuation.t
  -> dbg:Debuginfo.t
  -> simplify:Inlining_decision_intf.simplify
  -> simplify_apply_cont_to_cont:(
       ?don't_record_use:unit
    -> Simplify_env.t
    -> Simplify_result.t
    -> Continuation.t
    -> arg_tys:Flambda_type.t list
    -> Continuation.t * Simplify_result.t)
  -> inline_requested:Lambda.inline_attribute
  -> specialise_requested:Lambda.specialise_attribute
  -> Flambda.Expr.t * Simplify_result.t

(** When a function declaration is encountered by [for_call_site], the body
    may be subject to inlining immediately, thus changing the declaration.
    This function must return [true] for that to be able to happen. *)
val should_inline_inside_declaration : Flambda.Function_declaration.t -> bool