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

(** Union-find-like structure for keeping track of equivalence classes,
    used for alias resolution in the typing environment, with support for
    associating orderings to aliases of canonical elements. *)

[@@@ocaml.warning "+a-4-30-40-41-42"]

type t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : t

type add_result = private {
  t : t;
  canonical_element : Simple.t; (* has no coercion *)
  alias_of_demoted_element : Simple.t; (* has no coercion *)
  coercion_from_alias_of_demoted_to_canonical : Coercion.t;
}

val add
   : t
  -> element1:Simple.t
  -> binding_time_and_mode1:Binding_time.With_name_mode.t
  -> element2:Simple.t
  -> binding_time_and_mode2:Binding_time.With_name_mode.t
  -> add_result

val mem : t -> Simple.t -> bool

(** [get_canonical_element] returns [None] only when the
    [min_order_within_equiv_class] cannot be satisfied. *)
val get_canonical_element_exn
   : t
  -> Simple.t
  -> Name_mode.t
  -> min_name_mode:Name_mode.t
  -> Simple.t

module Alias_set : sig
  (** The set of aliases of one particular [Simple.t], or an intersection of
      such sets. *)
  type t

  val apply_coercion_to_all : t -> Coercion.t -> t option

  val inter : t -> t -> t

  val filter : t -> f:(Simple.t -> bool) -> t
  
  val to_list : t -> Simple.t list
end

(** [get_aliases] always returns the supplied element in the result map or
    as the constant. *)
val get_aliases
   : t
  -> Simple.t
  -> Alias_set.t

val get_canonical_ignoring_name_mode
   : t
  -> Name.t
  -> Simple.t

val merge : t -> t -> t

val clean_for_export : t -> t
