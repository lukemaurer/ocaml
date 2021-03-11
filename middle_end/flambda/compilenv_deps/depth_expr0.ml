
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

[@@@ocaml.warning "+a-30-40-41-42"]

type t = {
  offset : int;
  var : Depth_variable.t option;
  unroll_depth : int option;
}

let zero = { offset = 0; var = None; unroll_depth = None; }

let succ { offset; var; unroll_depth; } =
  let offset = offset + 1 in
  let unroll_depth =
    match unroll_depth with
    | None | Some 0 -> unroll_depth
    | Some n -> Some (n-1)
  in
  { offset; var; unroll_depth; }

let var var = { offset = 0; var = Some var; unroll_depth = None; }

let with_unroll_depth t unroll_depth =
  { t with unroll_depth = Some unroll_depth }

let print ppf { offset; var; unroll_depth } =
  let offset_and_var ppf () =
    match offset, var with
    | 0, Some var -> Depth_variable.print ppf var
    | n, None -> Format.fprintf ppf "%d" n
    | n, Some var -> Format.fprintf ppf "%d+%a" n Depth_variable.print var
  in
  match unroll_depth with
  | None -> offset_and_var ppf ()
  | Some unroll_depth ->
      Format.fprintf ppf "@[<hov 1>(%a@ @[<hov 1>(unroll_depth@ %d)@])@]"
        offset_and_var ()
        unroll_depth

type descr = t = {
  offset : int;
  var : Depth_variable.t option;
  unroll_depth : int option;
}

let descr t = t
