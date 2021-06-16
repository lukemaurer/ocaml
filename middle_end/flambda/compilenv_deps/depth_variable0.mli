(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2021 OCamlPro SAS                                    *)
(*   Copyright 2014--2021 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-9-30-40-41-42"]

module type S = sig
  module Variable : Identifiable.S

  type t = private Variable.t

  include Identifiable.S with type t := t

  (** Wrap a variable as a depth variable. The variable must have kind
      [Rec_info]. *)
  val of_var : Variable.t -> t

  val var : t -> Variable.t
end

module Make(Variable : Identifiable.S) : S with module Variable = Variable
