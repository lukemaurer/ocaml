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

[@@@ocaml.warning "+a-30-40-41-42"]

module TEE = Typing_env_extension

type t = Rec_info_expr.t

let print ppf t =
  Format.fprintf ppf "@[(Rec_info@ (%a))@]" Rec_info_expr.print t

let print_with_cache ~cache:_ ppf t = print ppf t

let apply_renaming t renaming = Rec_info_expr.apply_renaming t renaming

let free_names t = Rec_info_expr.free_names t

let all_ids_for_export t = Rec_info_expr.all_ids_for_export t

let apply_coercion t coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok t
  else Bottom

let eviscerate _ : _ Or_unknown.t = Unknown

let meet _env _t1 _t2 : _ Or_bottom.t =
  (* TODO *)
  Bottom

let join _env _t1 _t2 : _ Or_unknown.t =
  (* TODO *)
  Unknown
