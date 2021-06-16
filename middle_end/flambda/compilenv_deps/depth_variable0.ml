(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                       Pierre Chambart, OCamlPro                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2013--2020 OCamlPro SAS                                    *)
(*   Copyright 2014--2020 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

[@@@ocaml.warning "+a-30-40-41-42"]

module type S = sig
  module Variable : Identifiable.S

  type t = private Variable.t

  include Identifiable.S with type t := t

  (** Wrap a variable as a depth variable. The variable must have kind
      [Rec_info]. *)
  val of_var : Variable.t -> t

  val var : t -> Variable.t
end

module Make(Variable : Identifiable.S) : S with module Variable = Variable =
struct
  module Variable = Variable

  type t = Variable.t

  include Identifiable.Make(struct
    type nonrec t = t

    let compare = Variable.compare
    let hash = Variable.hash
    let equal = Variable.equal

    let print ppf t =
      Format.fprintf ppf "@<0>%s%a@<0>%s"
        (Flambda_colours.depth_variable ())
        Variable.print t
        (Flambda_colours.normal ())

    let output chan t =
      print (Format.formatter_of_out_channel chan) t
  end)

  let of_var t = t
  let var t = t
end
