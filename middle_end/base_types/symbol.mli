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

(** A symbol identifies a constant provided by either:
    - another compilation unit; or
    - a top-level module.

    * [sym_unit] is the compilation unit containing the value.
    * [sym_label] is the linkage name of the variable.

    The label must be globally unique: two compilation units linked in the
    same program must not share labels. *)

include Identifiable.S

type symbol = t

val create
   : Compilation_unit.t
  -> Linkage_name.t
  -> t

(* Create the symbol without prefixing with the compilation unit.
   Used for predefined exceptions *)
val unsafe_create
   : Compilation_unit.t
  -> Linkage_name.t
  -> t

val import_for_pack
   : t
  -> pack:Compilation_unit.t
  -> t

val compilation_unit : t -> Compilation_unit.t
val label : t -> Linkage_name.t

val print_opt : Format.formatter -> t option -> unit

val compare_lists : t list -> t list -> int

val in_compilation_unit : t -> Compilation_unit.t -> bool

val is_predefined_exception : t -> bool
