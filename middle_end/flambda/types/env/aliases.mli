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
  -> add_result Or_bottom.t

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

  val empty : t

  val singleton : Simple.t -> t

  val get_singleton : t -> Simple.t option

  val apply_coercion_to_all : t -> Coercion.t -> t Or_bottom.t

  val inter : t -> t -> t

  val filter : t -> f:(Simple.t -> bool) -> t

  (** Return the best alias in the set, where constants are better than
      symbols, which are better than variables, and ties are broken
      (arbitrarily) by [Simple.compare]. Returns [None] if the alias set is
      empty. *)
  val find_best : t -> Simple.t option

  val print : Format.formatter -> t -> unit
end

(** [get_aliases] always includes the supplied element in the alias set. *)
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
