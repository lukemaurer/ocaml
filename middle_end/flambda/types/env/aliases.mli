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

(* CR lmaurer: Maybe we should similarly have a type [canonical] isomorphic
   to [Simple.t]? *)
type coercion_to_canonical = {
  coercion_to_canonical : Coercion.t;
} [@@ocaml.unboxed]

type t

include Contains_ids.S with type t := t

val print : Format.formatter -> t -> unit

val invariant : t -> unit

val empty : t

type add_result = private {
  t : t;
  canonical_element : Simple.t;
  alias_of_demoted_element : Simple.t;
  coercion_from_alias_of_demoted_to_canonical : Coercion.t;
}

val add
   : t
  -> element1:Simple.t
  -> binding_time_and_mode1:Binding_time.With_name_mode.t
  -> coercion_from_element2_to_element1:Coercion.t
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
  -> Simple.t * coercion_to_canonical

(** [get_aliases] always returns the supplied element in the result set. *)
val get_aliases : t -> Simple.t -> coercion_to_canonical Simple.Map.t

val get_canonical_ignoring_name_mode
   : t
  -> Name.t
  -> Simple.t * coercion_to_canonical

val merge : t -> t -> t

