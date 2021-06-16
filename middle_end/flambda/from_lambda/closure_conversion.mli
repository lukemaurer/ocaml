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

(** Introduce closures into Ilambda code, producing Flambda. *)

module Env = Closure_conversion_aux.Env
module Acc = Closure_conversion_aux.Acc
module Function_decl = Closure_conversion_aux.Function_decls.Function_decl
module Expr_with_acc = Closure_conversion_aux.Expr_with_acc

val close_let
   : Acc.t -> Env.t -> Ident.t -> Ilambda.user_visible
  -> Ilambda.named
  -> body:(Acc.t -> Env.t -> Acc.t * Expr_with_acc.t)
  -> Acc.t * Expr_with_acc.t

val close_let_rec
   : Acc.t -> Env.t -> function_declarations:(Function_decl.t list)
  -> body:(Acc.t -> Env.t -> Acc.t * Expr_with_acc.t)
  -> Acc.t * Expr_with_acc.t

val close_let_cont
   : Acc.t -> Env.t -> name:Continuation.t -> is_exn_handler:bool
  -> params:((Ident.t * Ilambda.user_visible * Lambda.value_kind) list)
  -> recursive:Asttypes.rec_flag
  -> handler:(Acc.t -> Env.t -> Acc.t * Expr_with_acc.t)
  -> body:(Acc.t -> Env.t -> Acc.t * Expr_with_acc.t)
  -> Acc.t * Expr_with_acc.t

val close_apply
   : Acc.t -> Env.t -> Ilambda.apply
  -> Acc.t * Expr_with_acc.t

val close_apply_cont
   : Acc.t -> Env.t -> Continuation.t
  -> Ilambda.trap_action option -> Ilambda.simple list
  -> Acc.t * Expr_with_acc.t

val close_switch
   : Acc.t -> Env.t -> Ident.t -> Ilambda.switch
  -> Acc.t * Expr_with_acc.t

val ilambda_to_flambda
   : backend:(module Flambda_backend_intf.S)
  -> module_ident:Ident.t
  -> module_block_size_in_words:int
  -> Ilambda.program
  -> Flambda_unit.t
