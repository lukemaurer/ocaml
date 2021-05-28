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

let create name = Variable.create name
let of_var t = t
let var t = t
