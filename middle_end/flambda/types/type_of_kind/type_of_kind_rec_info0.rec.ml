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

type t = Rec_info_expr.t

let print ppf t = Rec_info_expr.print ppf t

let print_with_cache ~cache:_ ppf t = print ppf t

let apply_renaming t renaming = Rec_info_expr.apply_renaming t renaming

let free_names t = Rec_info_expr.free_names t

let all_ids_for_export t = Rec_info_expr.all_ids_for_export t

let apply_coercion t coercion : _ Or_bottom.t =
  if Coercion.is_id coercion then Ok t
  else Bottom

let eviscerate _ : _ Or_unknown.t = Unknown

let meet env t1 t2 : _ Or_bottom.t =
  if Rec_info_expr.equal t1 t2 then
    Ok (t1, Typing_env_extension.empty ())
  else begin
    if !Clflags.dump_rawflambda then begin
      Format.eprintf "@[<hov 1>Type_of_kind_rec_info.meet:@ %a@ ∧ %a@ = %a@ \
                      @[<hov 1>backtrace:@ %a@]@ \
                      @[<hov 1>env:@ %a@]@]@.%!"
        print t1
        print t2
        (Or_bottom.print print) Or_bottom.Bottom
        Misc.print_backtrace (Printexc.get_callstack 20)
        Meet_env.print env
    end;
    Bottom
  end

let join _env t1 t2 : _ Or_unknown.t =
  if Rec_info_expr.equal t1 t2 then Known t1 else begin
    if !Clflags.dump_rawflambda then begin
      Format.eprintf "@[<hov 1>join:@ %a@ ∨ %a@ = %a@]"
        print t1
        print t2
        (Or_unknown.print print) Or_unknown.Unknown
    end;
    Unknown
  end
