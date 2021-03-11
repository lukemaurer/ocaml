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

module Id = Table_by_int_id.Id

let depth_variable_flags = 0

module Data = struct
  type t = {
    compilation_unit : Compilation_unit.t;
    previous_compilation_units : Compilation_unit.t list;
    name : string;
    name_stamp : int;
  }

  let flags = depth_variable_flags

  let print ppf { compilation_unit; name; name_stamp;
                  previous_compilation_units = _; } =
    Format.fprintf ppf "@[<hov 1>(\
        @[<hov 1>(compilation_unit@ %a)@]@ \
        @[<hov 1>(name@ %s)@]@ \
        @[<hov 1>(name_stamp@ %d)@]@ \
        )@]"
      Compilation_unit.print compilation_unit
      name
      name_stamp

  let hash { compilation_unit; previous_compilation_units;
             name = _; name_stamp; } =
    Hashtbl.hash (List.map Compilation_unit.hash
                    (compilation_unit :: previous_compilation_units),
                  name_stamp)

  let equal t1 t2 =
    if t1 == t2 then true
    else
      let { compilation_unit = compilation_unit1; name_stamp = name_stamp1;
            previous_compilation_units = previous_compilation_units1;
            name = _
          } = t1
      in
      let { compilation_unit = compilation_unit2; name_stamp = name_stamp2;
            previous_compilation_units = previous_compilation_units2;
            name = _
          } = t2
      in
      let rec previous_compilation_units_match l1 l2 =
        match l1, l2 with
        | [], [] -> true
        | [], _ :: _ | _ :: _, [] -> false
        | unit1 :: tl1, unit2 :: tl2 ->
          Compilation_unit.equal unit1 unit2
          && previous_compilation_units_match tl1 tl2
      in
      Int.equal name_stamp1 name_stamp2
        && Compilation_unit.equal compilation_unit1 compilation_unit2
        && previous_compilation_units_match
             previous_compilation_units1
             previous_compilation_units2
end

type t = Id.t
type exported = Data.t

module Table = Table_by_int_id.Make(Data)
let grand_table_of_depth_variables = ref (Table.create ())

let initialise () = grand_table_of_depth_variables := Table.create ()

let previous_name_stamp = ref (-1)

let create name : t =
  let name_stamp =
    incr previous_name_stamp;
    !previous_name_stamp
  in
  let compilation_unit = Compilation_unit.get_current_exn () in
  let previous_compilation_units = [] in
  let data : Data.t =
    { compilation_unit; previous_compilation_units; name; name_stamp }
  in
  Table.add !grand_table_of_depth_variables data

let find_data t = Table.find !grand_table_of_depth_variables t

let rename t =
  let { Data.name; name_stamp = _; compilation_unit = _;
        previous_compilation_units = _;
      } = find_data t
  in
  create name

let name t = (find_data t).name

let name_stamp t = (find_data t).name_stamp

include Identifiable.Make (struct
  type nonrec t = t

  let compare t1 t2 =
    Id.compare t1 t2

  let equal t1 t2 =
    t1 == t2

  let hash t =
    Hashtbl.hash t

  let print ppf t =
    Format.fprintf ppf "@<0>%s" (Flambda_colours.depth_variable ());
    if String.equal (name t) "k"
    then Format.fprintf ppf "k%d" (name_stamp t)
    else Format.fprintf ppf "%s/%d" (name t) (name_stamp t);
    Format.fprintf ppf "@<0>%s" (Flambda_colours.normal ())

  let output chan t =
    print (Format.formatter_of_out_channel chan) t
end)

module Set = Patricia_tree.Make_set (struct let print = print end)
module Map = Patricia_tree.Make_map (struct let print = print end) (Set)
(* CR mshinwell: The [Tbl]s will still print integers! *)
module Tbl = Identifiable.Make_tbl (Numbers.Int) (Map)

let print_with_cache ~cache:_ ppf t = print ppf t

let export t = find_data t

let import data = Table.add !grand_table_of_depth_variables data

let map_compilation_unit f (data : Data.t) : Data.t =
  let new_compilation_unit = f data.compilation_unit in
  if Compilation_unit.equal new_compilation_unit data.compilation_unit
  then data
  else
    { data with compilation_unit = new_compilation_unit;
                previous_compilation_units =
                  data.compilation_unit :: data.previous_compilation_units;
    }
