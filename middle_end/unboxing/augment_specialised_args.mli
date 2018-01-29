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

(** Helper module for adding specialised arguments to sets of closures. *)

module Definition : sig
  type t =
    | Existing_inner_free_var of Variable.t
    | Projection_from_existing_specialised_arg of Projection.t
end

module What_to_specialise : sig
  type t

  val create
     : set_of_closures:Flambda.Set_of_closures.t
    -> t

  val new_specialised_arg
     : t
    -> fun_var:Variable.t
    -> group:Variable.t
    -> definition:Definition.t  (* [projecting_from] "existing inner vars" *)
    -> t

  val make_direct_call_surrogate_for : t -> fun_var:Variable.t -> t
end

module type S = sig
  val pass_name : string
  val variable_suffix : string

  val what_to_specialise
     : env:Simplify_aux.Env.t
    -> set_of_closures:Flambda.Set_of_closures.t
    -> What_to_specialise.t
end

module Make (T : S) : sig
  (** [duplicate_function] should be
      [Simplify.duplicate_function]. *)
  val rewrite_set_of_closures
     : env:Simplify_aux.Env.t
    -> duplicate_function:(
         env:Simplify_aux.Env.t
      -> set_of_closures:Flambda.Set_of_closures.t
      -> fun_var:Variable.t
      -> new_fun_var:Variable.t
      -> Flambda.Function_declaration.t
        * Flambda.specialised_to Variable.Map.t)
    -> set_of_closures:Flambda.Set_of_closures.t
    (* CR mshinwell: Add a type for "new let-bindings" like this? *)
    -> ((Variable.t * Flambda.Named.t) list
      * Flambda.Set_of_closures.t * Inlining_cost.Benefit.t) option
end