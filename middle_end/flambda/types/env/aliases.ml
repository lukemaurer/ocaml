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

[@@@ocaml.warning "+a-4-30-40-41-42"]

module Const = Reg_width_things.Const

type coercion_to_canonical = Coercion.t
type map_to_canonical = coercion_to_canonical Name.Map.t

let compose_map_values_exn map ~then_:coercion =
  if Coercion.is_id coercion then map else
    Name.Map.map (fun old_coercion ->
      Coercion.compose_exn old_coercion ~then_:coercion
    ) map

let compose_map_values map ~then_:coercion : _ Name.Map.t Or_bottom.t =
  if Coercion.is_id coercion then Ok map else
    let exception Return_bottom in
    match
      Name.Map.map (fun old_coercion ->
        match Coercion.compose old_coercion ~then_:coercion with
        | Some c -> c
        | None -> raise Return_bottom
      ) map
    with
    | exception Return_bottom -> Bottom
    | map -> Ok map

(* CR lmaurer: This should be in [Patricia_tree] *)
let map_keys f m =
  Name.Map.fold (fun name value acc ->
    Name.Map.add (f name) value acc)
    m
    Name.Map.empty

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant
     : t
    -> binding_times_and_modes:(Binding_time.With_name_mode.t Name.Map.t)
    -> unit

  val empty : t

  val add
     : t
    -> Name.t
    -> coercion_to_canonical
    -> Binding_time.With_name_mode.t
    -> t

  val earliest_alias :
    t -> min_name_mode:Name_mode.t option -> (Name.t * Coercion.t) option

  val all : t -> map_to_canonical

  val mem : t -> Name.t -> bool

  val union : t -> t -> t Or_bottom.t

  val disjoint : t -> t -> bool
  
  val import : (Name.t -> Name.t) -> t -> t

  val move_variables_to_mode_in_types : t -> t

  val apply_coercion_to_all : t -> Coercion.t -> t Or_bottom.t
end = struct
  module Earliest_alias : sig
    type t =
      | Earliest of {
          name : Name.t;
          coercion_to_canonical : Coercion.t;
          binding_time : Binding_time.t
        }
      | No_alias

    val exists : t -> bool
    val update : t -> Name.t -> coercion_to_canonical -> Binding_time.t -> t
    val union : t -> t -> t
    val map_name : t -> f:(Name.t -> Name.t) -> t
    val map_coercion : t -> f:(Coercion.t -> Coercion.t) -> t
    val print : Format.formatter -> t -> unit
  end = struct
    type t =
      | Earliest of {
          name : Name.t;
          coercion_to_canonical : Coercion.t;
          binding_time : Binding_time.t;
        }
      | No_alias

    let exists = function
      | Earliest _ -> true
      | No_alias -> false

    let update t new_name coercion_to_canonical binding_time =
      match t with
      | No_alias ->
        Earliest { name = new_name; coercion_to_canonical; binding_time }
      | Earliest { binding_time = old_binding_time; _ } ->
        if Binding_time.strictly_earlier binding_time ~than:old_binding_time
        then Earliest { name = new_name; coercion_to_canonical; binding_time }
        else t

    let union t1 t2 =
      match t2 with
      | No_alias -> t1
      | Earliest { name; coercion_to_canonical; binding_time } ->
        update t1 name coercion_to_canonical binding_time

    let map_name t ~f =
      match t with
      | Earliest e ->
        let name = f e.name in
        Earliest { e with name }
      | No_alias -> No_alias

    let map_coercion t ~f =
      match t with
      | Earliest e ->
        let coercion_to_canonical = f e.coercion_to_canonical in
        Earliest { e with coercion_to_canonical }
      | No_alias -> No_alias

    let print ppf = function
      | Earliest { name; coercion_to_canonical; binding_time } ->
        Format.fprintf ppf
          "@[<hov 1>(%a@ \
           @[<hov 1>@<0>%s(coercion_to_canonical@ %a)@<0>%s@]@ \
           @[<hov 1>(binding_time@ %a)@])@]"
          Name.print name
          (if Coercion.is_id coercion_to_canonical
           then Flambda_colours.elide ()
           else Flambda_colours.normal ())
          Coercion.print coercion_to_canonical
          (Flambda_colours.normal ())
          Binding_time.print binding_time
      | No_alias ->
        Format.pp_print_string ppf "<none>"
  end

  type t = {
    all : map_to_canonical;
    earliest : Earliest_alias.t;
    earliest_normal : Earliest_alias.t;
    (* Earliest alias whose name mode >= phantom (that is, normal or phantom) *)
    earliest_ge_phantom : Earliest_alias.t;
    (* Earliest alias whose name mode >= in-types *)
    earliest_ge_in_types : Earliest_alias.t;
  }

  let empty = {
    all = Name.Map.empty;
    earliest = No_alias;
    earliest_normal = No_alias;
    earliest_ge_phantom = No_alias;
    earliest_ge_in_types = No_alias;
  }

  let print ppf
        { earliest; earliest_normal; earliest_ge_phantom; earliest_ge_in_types;
          all } =
    let pp_earliest field_name ppf (earliest : Earliest_alias.t) =
      Format.fprintf ppf "@[<hov 1>@<0>%s(%s@ %a)@<0>%s@]"
        (if Earliest_alias.exists earliest 
         then Flambda_colours.normal ()
         else Flambda_colours.elide ())
        field_name
        Earliest_alias.print earliest
        (Flambda_colours.normal ())
    in  
    Format.fprintf ppf
      "@[<hov 1>(\
         %a@ %a@ %a@ %a@ \
         @[<hov 1>(all@ %a)@])\
         @]"
      (pp_earliest "earliest") earliest
      (pp_earliest "earliest_normal") earliest_normal
      (pp_earliest "earliest_ge_phantom") earliest_ge_phantom
      (pp_earliest "earliest_ge_in_types") earliest_ge_in_types
      (Name.Map.print Coercion.print) all
      
  let add t new_name coercion_to_canonical binding_time_and_name_mode =
    if Name.Map.mem new_name t.all then begin
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
                         %a"
        Name.print new_name
        print t
    end;
    let binding_time, name_mode =
      Binding_time.With_name_mode.(
        binding_time binding_time_and_name_mode,
        name_mode binding_time_and_name_mode)
    in
    let update earliest =
      Earliest_alias.update earliest new_name coercion_to_canonical
        binding_time
    in
    let update_if_mode_ge mode earliest =
      match Name_mode.compare_partial_order name_mode mode with
      | Some c when c >= 0 -> update earliest
      | _ -> earliest
    in
    let earliest = update t.earliest in
    let earliest_normal =
      update_if_mode_ge Name_mode.normal t.earliest_normal
    in
    let earliest_ge_phantom =
      update_if_mode_ge Name_mode.phantom t.earliest_ge_phantom
    in
    let earliest_ge_in_types =
      update_if_mode_ge Name_mode.in_types t.earliest_ge_in_types
    in
    let all = Name.Map.add new_name coercion_to_canonical t.all in
    { earliest; earliest_normal; earliest_ge_phantom; earliest_ge_in_types;
      all }

  let find_earliest t ~(min_name_mode : Name_mode.t option) =
    match min_name_mode with
    | None -> t.earliest
    | Some min_name_mode ->
      begin match Name_mode.descr min_name_mode with
      | Normal -> t.earliest_normal
      | Phantom -> t.earliest_ge_phantom
      | In_types -> t.earliest_ge_in_types
      end

  let invariant t ~binding_times_and_modes =
    let describe_field name_mode =
      match name_mode with
      | None -> "overall"
      | Some name_mode ->
        begin match Name_mode.descr name_mode with
          | Normal -> "normal"
          | Phantom -> "phantom (or normal)"
          | In_types -> "in-types (or normal)"
        end
    in
    let check name binding_time name_mode (earliest : Earliest_alias.t) =
      match earliest with
      | No_alias -> ()
      | Earliest e as earliest_as_recorded ->
        if Binding_time.compare binding_time e.binding_time < 0 then
          Misc.fatal_errorf
            "@[<hov 1>Earliest %s alias %a@ has binding time %a,@ \
             earlier than %a@ in %a\
             @]"
           (describe_field name_mode)
           Name.print name
           Binding_time.print binding_time
           Earliest_alias.print earliest_as_recorded
           print t
    in
    Name.Map.iter (fun name _coercion ->
      let binding_time_and_mode =
        Name.Map.find name binding_times_and_modes
      in
      let binding_time, name_mode =
        Binding_time.With_name_mode.(
          binding_time binding_time_and_mode,
          name_mode binding_time_and_mode)
      in  
      check name binding_time None t.earliest;
      let earliest_in_mode = find_earliest t ~min_name_mode:(Some name_mode) in
      check name binding_time (Some name_mode) earliest_in_mode
    ) t.all;
    let check_earliest min_name_mode =
      match find_earliest t ~min_name_mode with
      | No_alias -> ()
      | Earliest e as earliest ->
        let in_map =
          match Name.Map.find_opt e.name t.all with
          | None -> false
          | Some coercion -> Coercion.equal coercion e.coercion_to_canonical
        in
        if not in_map then begin
          Misc.fatal_errorf
            "@[<v>Aliases_of_canonical_element: Earliest %s not in map@ \
             @[<hov 1>Alias: %a@ Map: %a@]@]"
            (describe_field min_name_mode)
            Earliest_alias.print earliest
            (Name.Map.print Coercion.print) t.all
        end
    in
    List.iter check_earliest
      [ None; 
        Some Name_mode.normal;
        Some Name_mode.phantom;
        Some Name_mode.in_types ]

  let mem t elt =
    Name.Map.mem elt t.all

  let all t = t.all

  let earliest_alias t ~min_name_mode : (Name.t * Coercion.t) option =
    match find_earliest t ~min_name_mode with
    | Earliest { name; coercion_to_canonical; binding_time = _ } ->
      Some (name, coercion_to_canonical)
    | No_alias -> None

  let union t1 t2 : t Or_bottom.t =
    let exception Return_bottom in
    match
      Name.Map.merge (fun _name coercion1 coercion2 ->
        match coercion1, coercion2 with
        | None, None -> assert false
        | Some coercion, None
        | None, Some coercion -> Some coercion
        | Some coercion1, Some coercion2 ->
          if Coercion.equal coercion1 coercion2 then Some coercion1
          else raise Return_bottom
      ) t1.all t2.all
    with
    | exception Return_bottom -> Bottom
    | all ->
      let earliest =
        Earliest_alias.union t1.earliest t2.earliest
      in
      let earliest_normal =
        Earliest_alias.union t1.earliest_normal t2.earliest_normal
      in
      let earliest_ge_phantom =
        Earliest_alias.union t1.earliest_ge_phantom t2.earliest_ge_phantom
      in
      let earliest_ge_in_types =
        Earliest_alias.union t1.earliest_ge_in_types t2.earliest_ge_in_types
      in
      Ok { all; earliest; earliest_normal; earliest_ge_phantom;
           earliest_ge_in_types; }

  let disjoint t1 t2 =
    not (Name.Map.inter_domain_is_non_empty t1.all t2.all)

  let update_all_earliest
        { all; earliest; earliest_normal;
          earliest_ge_phantom; earliest_ge_in_types; } ~f =
    let earliest = f earliest in
    let earliest_normal = f earliest_normal in
    let earliest_ge_phantom = f earliest_ge_phantom in
    let earliest_ge_in_types = f earliest_ge_in_types in
    { all; earliest; earliest_normal; earliest_ge_phantom;
      earliest_ge_in_types; }

  let import import_name
        ({ all; earliest = _; earliest_normal = _; earliest_ge_phantom = _;
           earliest_ge_in_types = _ } as t) =
    let all = map_keys import_name all in
    let t = { t with all; } in
    let t = update_all_earliest t ~f:(Earliest_alias.map_name ~f:import_name) in
    t

  let apply_coercion_to_all t coercion : t Or_bottom.t =
    compose_map_values t.all ~then_:coercion
    |>
    Or_bottom.map ~f:(fun all ->
      let t = { t with all; } in
      let t = update_all_earliest t ~f:(fun earliest ->
        (* This composition must succeed if coerce_map did *)
        Earliest_alias.map_coercion earliest ~f:(fun coercion_to_canonical ->
          Coercion.compose_exn coercion_to_canonical ~then_:coercion))
      in
      t)

  let move_variables_to_mode_in_types t =
    update_all_earliest t ~f:(fun earliest ->
      match earliest with
      | Earliest { name; _ } when Name.is_var name -> No_alias
      | _ -> earliest)
end

module Alias_set = struct
  type t = {
    const : Const.t option;
    names : Coercion.t Name.Map.t;
  }

  let empty = { const = None; names = Name.Map.empty; }

  let singleton simple =
    Simple.pattern_match simple
      ~const:(fun const ->
        { const = Some const; names = Name.Map.empty; })
      ~name:(fun name ->
        let coercion = Simple.coercion simple in
        { const = None; names = Name.Map.singleton name coercion })

  let get_singleton { const; names; } =
    match const with
    | Some const ->
      if Name.Map.is_empty names then Some (Simple.const const) else None
    | None ->
      Name.Map.get_singleton names
      |> Option.map (fun (name, coercion) ->
           Simple.with_coercion (Simple.name name) coercion)

  let print ppf { const; names; } =
    let none ppf () =
      Format.fprintf ppf "@<0>%s()" (Flambda_colours.elide ())
    in
    Format.fprintf ppf
      "@[<hov 1>(\
           @[<hov 1>(const@ %a)@]@ \
           @[<hov 1>(names@ %a)@]@ \
       @]"
       (Format.pp_print_option Const.print ~none) const
       (Name.Map.print Coercion.print) names

  let apply_coercion_to_all { const; names; } coercion =
    compose_map_values names ~then_:coercion
    |>
    Or_bottom.map ~f:(fun names -> { const; names; })

  let inter
        { const = const1; names = names1; }
        { const = const2; names = names2; } =
    let const =
      match const1, const2 with
      | Some const1, Some const2 when Const.equal const1 const2 -> Some const1        | _, _ -> None
    in
    let names =
      Name.Map.merge (fun _name coercion1 coercion2 ->
        match coercion1, coercion2 with
        | Some coercion1, Some coercion2
            when Coercion.equal coercion1 coercion2 -> Some coercion1
        | _, _ -> None
      ) names1 names2
    in
    { const; names; }

  let filter { const; names; } ~f =
    let const =
      match const with
      | Some const when f (Simple.const const) -> Some const
      | _ -> None
    in
    let names =
      Name.Map.filter (fun name coercion ->
        let simple = Simple.with_coercion (Simple.name name) coercion in
        f simple
      ) names
    in
    { const; names; }

  let find_best { const; names; } =
    match const with
    | Some const -> Some (Simple.const const)
    | None ->
      let key_is_symbol key _ = Name.is_symbol key in
      let (symbols, vars) = Name.Map.partition key_is_symbol names in
      let simple_of_binding (name, coercion) =
        Simple.with_coercion (Simple.name name) coercion
      in
      match Name.Map.min_binding_opt symbols with
      | Some binding ->
        Some (binding |> simple_of_binding)
      | None ->
        match Name.Map.min_binding_opt vars with
        | Some binding ->
          Some (binding |> simple_of_binding)
        | None ->
          None
end

type t = {
  canonical_elements : (Simple.t * coercion_to_canonical) Name.Map.t;
  (* Canonical elements that have no known aliases may not be included in
     [canonical_elements]. (This comment used to say "are not included," but
     this was not the case; any code that relied on this invariant should be
     fixed.) *)
  aliases_of_canonical_names : Aliases_of_canonical_element.t Name.Map.t;
  (* For [elt |-> aliases] in [aliases_of_canonical_names], then
     [aliases] never includes [elt]. *)
  (* CR mshinwell: check this always holds *)
  aliases_of_consts : Aliases_of_canonical_element.t Const.Map.t;
  binding_times_and_modes : Binding_time.With_name_mode.t Name.Map.t;
  (* Binding times and name modes define an order on the elements.
     The canonical element for a set of aliases is always the minimal
     element for this order, which is different from the order used
     for creating sets and maps. *)
}

(* Canonical elements can be seen as a collection of star graphs:

   canon_i <--[coercion_i_0]-- elem_i_0
       ^ ^--[...]-- ...
        \--[coercion_i_m]-- elem_i_m

   ...

   canon_j <--[coercion_j_0]-- elem_j_0
       ^ ^--[...]-- ...
        \--[coercion_j_n]-- elem_j_n


   stored as a map:

   canonical_elements[elem_i_0] = (canon_i, coercion_i_0)
   ...
   canonical_elements[elem_i_m] = (canon_i, coercion_i_m)

   ...

   canonical_elements[elem_j_0] = (canon_j, coercion_j_0)
   ...
   canonical_elements[elem_j_n] = (canon_j, coercion_j_n)
*)

let print ppf { canonical_elements; aliases_of_canonical_names;
                aliases_of_consts; binding_times_and_modes; } =
  let print_element_and_coercion ppf (elt, coercion) =
    Format.fprintf ppf "@[<hov 1>(\
                        %a@ \
                        @[<hov 1>@<0>%s(coercion@ %a)@<0>%s@]\
                        )@]"
      Simple.print elt
      (if Coercion.is_id coercion
      then Flambda_colours.elide ()
      else Flambda_colours.normal ())
      Coercion.print coercion
      (Flambda_colours.normal ())
  in
  Format.fprintf ppf
    "@[<hov 1>(\
     @[<hov 1>(canonical_elements@ %a)@]@ \
     @[<hov 1>(aliases_of_canonical_names@ %a)@]@ \
     @[<hov 1>(aliases_of_consts@ %a)@]@ \
     @[<hov 1>(binding_times_and_modes@ %a)@]\
     )@]"
    (Name.Map.print print_element_and_coercion) canonical_elements
    (Name.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_names
    (Const.Map.print Aliases_of_canonical_element.print)
    aliases_of_consts
    (Name.Map.print Binding_time.With_name_mode.print)
    binding_times_and_modes

let name_defined_earlier t alias ~than =
  let info1 = Name.Map.find alias t.binding_times_and_modes in
  let info2 = Name.Map.find than t.binding_times_and_modes in
  Binding_time.strictly_earlier
    (Binding_time.With_name_mode.binding_time info1)
    ~than:(Binding_time.With_name_mode.binding_time info2)

let defined_earlier t alias ~than =
  Simple.pattern_match than
    ~const:(fun _ -> false)
    ~name:(fun than ->
      Simple.pattern_match alias
        ~const:(fun _ -> true)
        ~name:(fun alias -> name_defined_earlier t alias ~than))

let name_mode t elt =
  Simple.pattern_match elt
    ~const:(fun _ -> Name_mode.normal)
    ~name:(fun elt ->
      Binding_time.With_name_mode.name_mode
        (Name.Map.find elt t.binding_times_and_modes))

let binding_time_and_name_mode t elt =
  Simple.pattern_match elt
    ~const:(fun _ -> 
      Binding_time.With_name_mode.create Binding_time.consts_and_discriminants
      Name_mode.normal)
    ~name:(fun elt ->
      Name.Map.find elt t.binding_times_and_modes)

let invariant t =
  if !Clflags.flambda_invariant_checks then begin
    let _all_aliases : map_to_canonical =
      Name.Map.fold (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases
            ~binding_times_and_modes:t.binding_times_and_modes;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not (Name.Map.for_all (fun elt _coercion ->
              defined_earlier t (Simple.name canonical_element)
                ~than:(Simple.name elt))
            aliases)
          then begin
            Misc.fatal_errorf "Canonical element %a is not earlier than \
                all of its aliases:@ %a"
              Name.print canonical_element
              print t
          end;
          if Name.Map.mem canonical_element aliases then begin
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Name.print canonical_element
              (Name.Map.print Coercion.print) aliases
          end;
          if Name.Map.inter_domain_is_non_empty aliases all_aliases then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          Name.Map.disjoint_union aliases all_aliases)
        t.aliases_of_canonical_names
        Name.Map.empty
    in
    ()
  end

let empty = {
  (* CR mshinwell: Rename canonical_elements, maybe to
     aliases_to_canonical_elements. *)
  canonical_elements = Name.Map.empty;
  aliases_of_canonical_names = Name.Map.empty;
  aliases_of_consts = Const.Map.empty;
  binding_times_and_modes = Name.Map.empty;
}

type canonical =
  | Is_canonical
  | Alias_of_canonical of {
      canonical_element : Simple.t;
      coercion_to_canonical : coercion_to_canonical;
    }

let canonical t element : canonical =
  Simple.pattern_match element
    ~const:(fun _ -> Is_canonical)
    ~name:(fun name ->
      match Name.Map.find name t.canonical_elements with
      | exception Not_found -> Is_canonical
      | canonical_element, coercion_from_name_to_canonical_element ->
        let coercion_from_name_to_element = Simple.coercion element in
        if !Clflags.flambda_invariant_checks then begin
          assert (not (Simple.equal element canonical_element))
        end;
        let coercion_from_element_to_canonical_element =
          Coercion.compose_exn
            (Coercion.inverse coercion_from_name_to_element)
            ~then_:coercion_from_name_to_canonical_element
        in
        let coercion_to_canonical =
          coercion_from_element_to_canonical_element
        in
        Alias_of_canonical { canonical_element; coercion_to_canonical; })

let get_aliases_of_canonical_element t ~canonical_element =
  if !Clflags.flambda_invariant_checks then begin
    assert (Coercion.is_id (Simple.coercion canonical_element))
  end;
  let name name =
    Name.Map.find name t.aliases_of_canonical_names
  in
  let const const =
    Const.Map.find const t.aliases_of_consts
  in
  match Simple.pattern_match canonical_element ~name ~const with
  | exception Not_found -> Aliases_of_canonical_element.empty
  | aliases -> aliases

(*
   before
   ~~~~~~
   canonical_element <--[coercion_ce_0]-- ce_0
     ^ ^--[...]-- ...
      \--[coercion_ce_m]-- ce_m
   to_be_demoted <--[coercion_tbd_0]-- tbd_0
     ^ ^--[...]-- ...
      \--[coercion_tbd_n]-- tbd_n

   i.e.

   canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
   ...
   canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
   canonical_elements[tbd_0] = (to_be_demoted, coercion_tbd_0)
   ...
   canonical_elements[tbd_n] = (to_be_demoted, coercion_tbd_n)


   after
   ~~~~~
   canonical_element <--[coercion_ce_0]-- ce_0
     ^ ^ ^ ^ ^ ^--[...]-- ...
     | | | |  \--[coercion_ce_m]-- ce_m
     | | | \--[coercion_to_canonical]-- to_be_demoted
     | | \--[compose(coercion_tbd_0, coercion_to_canonical)]-- tbd_0
     | \--[...]-- ...
     \--[compose(coercion_tbd_n, coercion_to_canonical)]-- tbd_n

   i.e.

   canonical_elements[ce_0] = (canonical_element, coercion_ce_0)
   ...
   canonical_elements[ce_m] = (canonical_element, coercion_ce_m)
   canonical_elements[to_be_demoted] = (canonical_element, coercion_to_canonical)
   canonical_elements[tbd_0] = (canonical_element, compose(coercion_tbd_0, coercion_to_canonical))
   ...
   canonical_elements[tbd_n] = (canonical_element, compose(coercion_tbd_n, coercion_to_canonical))

*)
let add_alias_between_canonical_elements t ~canonical_element ~coercion_to_canonical ~to_be_demoted
    : _ Or_bottom.t =
  if Simple.equal canonical_element to_be_demoted then begin
    if Coercion.is_id coercion_to_canonical then begin
      Ok t
    end else
      Misc.fatal_errorf
        "Cannot add an alias of %a@ to itself with a non-identity coercion@ %a"
        Simple.print canonical_element
        Coercion.print coercion_to_canonical
  end else
    let name_to_be_demoted =
      Simple.pattern_match to_be_demoted
        ~const:(fun c ->
          Misc.fatal_errorf
            "Can't be demoting const %a@ while adding alias to@ %a"
          Const.print c
          Simple.print canonical_element)
        ~name:(fun name -> name)
    in
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if !Clflags.flambda_invariant_checks then begin
      Simple.pattern_match canonical_element
        ~const:(fun _ -> ())
        ~name:(fun canonical_element ->
          assert (not (Aliases_of_canonical_element.mem
            aliases_of_to_be_demoted canonical_element)))
    end;
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element name_to_be_demoted));
      assert (Aliases_of_canonical_element.disjoint 
        aliases_of_canonical_element aliases_of_to_be_demoted)
    end;
    let ( let* ) x f = Or_bottom.bind x ~f in
    let* aliases = 
      Aliases_of_canonical_element.apply_coercion_to_all
        aliases_of_to_be_demoted coercion_to_canonical
    in
    let* aliases =
      Aliases_of_canonical_element.union aliases aliases_of_canonical_element
    in
    let aliases =
      Aliases_of_canonical_element.add
        aliases
        name_to_be_demoted
        coercion_to_canonical
        (binding_time_and_name_mode t to_be_demoted)
    in
    let aliases_of_canonical_names =
      Name.Map.remove name_to_be_demoted t.aliases_of_canonical_names
    in
    let aliases_of_canonical_names, aliases_of_consts =
      Simple.pattern_match canonical_element
        ~name:(fun name ->
          Name.Map.add (* replace *) name aliases aliases_of_canonical_names,
          t.aliases_of_consts)
        ~const:(fun const ->
          aliases_of_canonical_names,
          Const.Map.add (* replace *) const aliases t.aliases_of_consts)
    in
    let canonical_elements =
      t.canonical_elements
      |> Name.Map.fold (fun alias coercion_to_to_be_demoted canonical_elements ->
          let coercion_to_canonical =
            (* This shouldn't throw an exception, since the previous
               compositions succeeded *)
            Coercion.compose_exn coercion_to_to_be_demoted
              ~then_:coercion_to_canonical
          in
          Name.Map.add alias (canonical_element, coercion_to_canonical)
            canonical_elements)
        (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Name.Map.add name_to_be_demoted
        (canonical_element, coercion_to_canonical)
    in
    let res =
    { canonical_elements;
      aliases_of_canonical_names;
      aliases_of_consts;
      binding_times_and_modes = t.binding_times_and_modes;
    } in
    invariant res;
    Ok res

type to_be_demoted = Demote_canonical_element1 | Demote_canonical_element2

let choose_canonical_element_to_be_demoted t ~canonical_element1
      ~canonical_element2 =
  if defined_earlier t canonical_element1 ~than:canonical_element2
  then Demote_canonical_element2 else Demote_canonical_element1

(* CR mshinwell: add submodule *)
type add_result = {
  t : t;
  canonical_element : Simple.t;
  demoted_alias : Simple.t;
}

let invariant_add_result ~original_t { canonical_element; demoted_alias; t; } =
  if !Clflags.flambda_invariant_checks then begin
    invariant t;
    if not (Simple.equal canonical_element demoted_alias) then begin
      if not (defined_earlier t canonical_element
                ~than:demoted_alias) then begin
        Misc.fatal_errorf "Canonical element %a should be defined earlier \
                           than %a after alias addition.@ Original alias tracker:@ %a@ \
                           Resulting alias tracker:@ %a"
          Simple.print canonical_element
          Simple.print demoted_alias
          print original_t
          print t
      end
    end
  end

let add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2 =
  let add ~canonical_element1 ~canonical_element2
        ~coercion_from_element1_to_canonical_element1
        ~coercion_from_element2_to_canonical_element2
        ~coercion_from_canonical_element2_to_canonical_element1 =
    let canonical_element, demoted_canonical, demoted_alias,
        coercion_from_demoted_canonical_to_canonical,
        coercion_from_demoted_alias_to_demoted_canonical = 
      let which_element =
        choose_canonical_element_to_be_demoted t
          ~canonical_element1 ~canonical_element2
      in
      match which_element with
      | Demote_canonical_element1 ->
        let coercion_from_canonical_element1_to_canonical_element2 =
          Coercion.inverse
            coercion_from_canonical_element2_to_canonical_element1
        in
        canonical_element2, canonical_element1, element1,
        coercion_from_canonical_element1_to_canonical_element2,
        coercion_from_element1_to_canonical_element1
      | Demote_canonical_element2 ->
        canonical_element1, canonical_element2, element2,
        coercion_from_canonical_element2_to_canonical_element1,
        coercion_from_element2_to_canonical_element2
    in
    let t = 
      add_alias_between_canonical_elements
        t
        ~canonical_element
        ~coercion_to_canonical:coercion_from_demoted_canonical_to_canonical
        ~to_be_demoted:demoted_canonical
    in
    let coercion_from_demoted_alias_to_canonical =
      Coercion.compose_exn
        coercion_from_demoted_alias_to_demoted_canonical
        ~then_:coercion_from_demoted_canonical_to_canonical
    in
    let demoted_alias =
      Simple.with_coercion demoted_alias
        coercion_from_demoted_alias_to_canonical
    in
    Or_bottom.map t ~f:(fun t ->
      { t;
        canonical_element;
        demoted_alias;
      })
  in
  match canonical t element1, canonical t element2 with
  | Is_canonical, Is_canonical ->
    let canonical_element1 = element1 in
    let canonical_element2 = element2 in
    let coercion_from_element1_to_canonical_element1 = Coercion.id in
    let coercion_from_element2_to_canonical_element2 = Coercion.id in
    let coercion_from_canonical_element2_to_canonical_element1 =
      coercion_from_element2_to_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Alias_of_canonical {
      canonical_element = canonical_element1;
      coercion_to_canonical = coercion_from_element1_to_canonical_element1;
    },
    Is_canonical ->
    let canonical_element2 = element2 in
    let coercion_from_element2_to_canonical_element2 = Coercion.id in
    (* element1 <--[c]-- canonical_element2=element2
       +
       canonical_element1 <--[c1] element1
       ~>
       canonical_element1 <--[c1 << c]-- canonical_element2 *)
    let coercion_from_canonical_element2_to_canonical_element1 =
      Coercion.compose_exn coercion_from_element2_to_element1
        ~then_:coercion_from_element1_to_canonical_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Is_canonical,
    Alias_of_canonical {
      canonical_element = canonical_element2;
      coercion_to_canonical = coercion_from_element2_to_canonical_element2;
    } ->
    let canonical_element1 = element1 in
    let coercion_from_element1_to_canonical_element1 = Coercion.id in
    let coercion_from_canonical_element2_to_canonical_element1 =
      (* canonical_element1=element1 <--[c]-- element2 
         +
         canonical_element2 <--[c2]-- element2
         ~>
         element2 <--[c2^-1]-- canonical_element2
         ~>
         canonical_element1 <--[c << c2^-1]-- canonical_element2
      *)
      Coercion.compose_exn
        (Coercion.inverse coercion_from_element2_to_canonical_element2)
        ~then_:coercion_from_element2_to_element1
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1
  | Alias_of_canonical {
      canonical_element = canonical_element1;
      coercion_to_canonical = coercion_from_element1_to_canonical_element1;
    },
    Alias_of_canonical {
      canonical_element = canonical_element2;
      coercion_to_canonical = coercion_from_element2_to_canonical_element2;
    } ->
    let coercion_from_canonical_element2_to_canonical_element1 =
      (* canonical_element1 <--[c1]-- element1
         canonical_element2 <--[c2]-- element2
         +
         element1 <--[c]-- element2
         ~>
         element2 <--[c2^-1]-- canonical_element2
         ~>
         canonical_element1 <--[c1 << c << c2^-1]-- canonical_element2
         *)
      Coercion.compose_exn
        (Coercion.inverse coercion_from_element2_to_canonical_element2)
        ~then_:(Coercion.compose_exn
          coercion_from_element2_to_element1
          ~then_:coercion_from_element1_to_canonical_element1)
    in
    add ~canonical_element1 ~canonical_element2
      ~coercion_from_element1_to_canonical_element1
      ~coercion_from_element2_to_canonical_element2
      ~coercion_from_canonical_element2_to_canonical_element1

let add t ~element1:element1_with_coercion ~binding_time_and_mode1
      ~element2:element2_with_coercion ~binding_time_and_mode2 =
  Simple.pattern_match element1_with_coercion
    ~name:(fun _ -> ())
    ~const:(fun const1 ->
      Simple.pattern_match element2_with_coercion
        ~name:(fun _ -> ())
        ~const:(fun const2 ->
          if not (Const.equal const1 const2) then begin
            Misc.fatal_errorf
              "Cannot add alias between two non-equal consts: %a <> %a"
                Const.print const1
                Const.print const2
          end));
  let original_t = t in
  (* element1_with_coercion <--[c1]-- element1
     +
     element2_with_coercion <--[c2]-- element2
     ~
     element1 <--[c1^-1]-- element1_with_coercion
     ~
     element1 <--[c1^-1 << c2]-- element2
   *)
  let element1 = element1_with_coercion |> Simple.without_coercion in
  let element2 = element2_with_coercion |> Simple.without_coercion in
  let coercion_from_element2_to_element1 =
    Coercion.compose_exn (Simple.coercion element2_with_coercion)
      ~then_:(Coercion.inverse (Simple.coercion element1_with_coercion))
  in
  let add_if_name simple data map =
    Simple.pattern_match simple
      ~const:(fun _ -> map)
      ~name:(fun name -> Name.Map.add name data map)
  in
  let t =
    { t with binding_times_and_modes =
               add_if_name element1 binding_time_and_mode1
                 (add_if_name element2 binding_time_and_mode2
                    t.binding_times_and_modes);
    }
  in
  let add_result : add_result Or_bottom.t =
    add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2
  in
  if !Clflags.flambda_invariant_checks then begin
    match add_result with
    | Ok add_result -> invariant_add_result ~original_t add_result
    | Bottom -> ()
  end;
  add_result

let mem t element =
  Simple.pattern_match element
    ~const:(fun const ->
      Const.Map.mem const t.aliases_of_consts)
    ~name:(fun name ->
      Name.Map.mem name t.binding_times_and_modes)

  (* CR mshinwell: This needs documenting.  For the moment we allow
     relations between canonical elements that are actually incomparable
     under the name mode ordering, and check in [get_canonical_element_exn]
     accordingly.  However maybe we should never allow these situations to
     arise. *)
  (*
  let canonical_mode =
    name_mode t add_result.canonical_element
  in
  let alias_of_mode = name_mode t add_result.alias_of in
  match
    Name_mode.compare_partial_order
      canonical_mode alias_of_mode
  with
  | Some _ -> add_result
  | None ->
    Misc.fatal_errorf "Canonical %a has mode incomparable with %a in:@ %a"
      Simple.print add_result.canonical_element
      Simple.print add_result.alias_of
      print t
  *)

let get_canonical_element_exn t element elt_name_mode ~min_name_mode =
  let canonical_element, name_mode, coercion_from_canonical_to_element =
    match canonical t element with
    | Is_canonical ->
      element, elt_name_mode, Coercion.id
    | Alias_of_canonical { canonical_element; coercion_to_canonical; } ->
      let name_mode = name_mode t canonical_element in
      canonical_element, name_mode, Coercion.inverse coercion_to_canonical
  in
  (*
  if !Clflags.flambda_invariant_checks then Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
    Simple.print element
    Simple.print canonical_element
    Name_mode.print min_name_mode;
  *)
  let find_earliest () =
    (* There used to be a shortcut that avoided a consulting the aliases in the
       common case that [element] is itself canonical and has no aliases, since
       then it does not appear in [canonical_elements]. However, this shortcut
       was broken: a canonical element *with* known aliases may still not appear
       in [canonical_elements]. See tests/flambda2-aliases for a test that gave
       incorrect output (saying x/39 had no aliases). It may be worth restoring
       the shortcut, perhaps by returning more information from [canonical]. *)
    let aliases = get_aliases_of_canonical_element t ~canonical_element in
    match
      Aliases_of_canonical_element.earliest_alias aliases
        ~min_name_mode:(Some min_name_mode)
    with
    | Some (earliest, coercion_from_earliest_to_canonical) ->
      let coercion_from_earliest_to_element =
        Coercion.compose_exn coercion_from_earliest_to_canonical
          ~then_:coercion_from_canonical_to_element
      in
      Simple.with_coercion (Simple.name earliest)
        coercion_from_earliest_to_element
    | None -> raise Not_found
  in
  match
    Name_mode.compare_partial_order name_mode min_name_mode
  with
  | None -> find_earliest ()
  | Some c ->
    if c >= 0 then
      Simple.with_coercion canonical_element coercion_from_canonical_to_element
    else find_earliest ()

let make_get_aliases_result
    ~element:_
    ~canonical_element
    ~coercion_from_canonical_to_element
    ~(names_with_coercions_to_element : Coercion.t Name.Map.t) =
  Simple.pattern_match canonical_element
    ~const:(fun canonical_const ->
      let const = Some canonical_const in
      let names = names_with_coercions_to_element in
      { Alias_set.const; names })
    ~name:(fun canonical_name ->
      let names =
        Name.Map.add canonical_name coercion_from_canonical_to_element
          names_with_coercions_to_element
      in
      { Alias_set.const = None; names })

let get_aliases t element =
  match canonical t element with
  | Is_canonical ->
    let canonical_element = element in
    let names_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    let names_with_coercions_to_element =
      names_with_coercions_to_canonical
    in
    let coercion_from_canonical_to_element = Coercion.id in
    make_get_aliases_result
      ~element
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~names_with_coercions_to_element
  | Alias_of_canonical { canonical_element;
      coercion_to_canonical = coercion_from_element_to_canonical; } ->
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Simple.equal element canonical_element))
    end;
    
    let names_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
       (get_aliases_of_canonical_element t ~canonical_element)
    in
    let coercion_from_canonical_to_element =
      Coercion.inverse coercion_from_element_to_canonical
    in
    (* If any composition fails, then our coercions are inconsistent somehow,
       which should only happen when meeting *)
    let names_with_coercions_to_element =
      compose_map_values_exn names_with_coercions_to_canonical
        ~then_:coercion_from_canonical_to_element
    in

    if !Clflags.flambda_invariant_checks then begin
      let element_coerced_to_canonical =
        Simple.apply_coercion_exn element coercion_from_element_to_canonical
      in
      (* These aliases are all equivalent to the canonical element, and so is
         our original [element] if we coerce it first, so the coerced form of
         [element] should be among the aliases. *)
      assert (Name.Map.exists 
        (fun name coercion_from_name_to_canonical ->
          let name_coerced_to_canonical =
            Simple.apply_coercion_exn
              (Simple.name name)
              coercion_from_name_to_canonical
          in
          Simple.equal element_coerced_to_canonical name_coerced_to_canonical
        ) names_with_coercions_to_canonical)
    end;

    make_get_aliases_result
      ~element
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~names_with_coercions_to_element

let all_ids_for_export { canonical_elements = _;
                         aliases_of_canonical_names = _;
                         aliases_of_consts;
                         binding_times_and_modes; } =
  let ids = Ids_for_export.empty in
  let ids =
    Name.Map.fold (fun elt _binding_time_and_mode ids ->
      Ids_for_export.add_name ids elt)
      binding_times_and_modes
      ids
  in
  let ids =
    Const.Map.fold (fun elt _aliases ids ->
      Ids_for_export.add_const ids elt)
      aliases_of_consts
      ids
  in
  ids

let import import_map { canonical_elements;
                        aliases_of_canonical_names;
                        aliases_of_consts;
                        binding_times_and_modes } =
  let import_simple x = Ids_for_export.Import_map.simple import_map x in
  let import_name x = Ids_for_export.Import_map.name import_map x in
  let import_const c = Ids_for_export.Import_map.const import_map c in
  let canonical_elements =
    Name.Map.fold (fun elt (canonical, coercion) acc ->
      Name.Map.add (import_name elt) (import_simple canonical, coercion) acc)
      canonical_elements
      Name.Map.empty
  in
  let aliases_of_canonical_names =
    Name.Map.fold (fun canonical aliases acc ->
        Name.Map.add (import_name canonical)
          (Aliases_of_canonical_element.import import_name aliases)
          acc)
      aliases_of_canonical_names
      Name.Map.empty
  in
  let aliases_of_consts =
    Const.Map.fold (fun const aliases acc ->
        Const.Map.add (import_const const)
          (Aliases_of_canonical_element.import import_name aliases)
          acc)
      aliases_of_consts
      Const.Map.empty
  in
  let binding_times_and_modes =
    map_keys import_name binding_times_and_modes
  in
  let t =
    { canonical_elements;
      aliases_of_canonical_names;
      aliases_of_consts;
      binding_times_and_modes;
    }
  in
  invariant t;
  t

let merge t1 t2 =
  let canonical_elements =
    Name.Map.disjoint_union
      t1.canonical_elements
      t2.canonical_elements
  in
  (* Warning: we assume that the aliases in the two alias trackers are disjoint,
     but nothing stops them from sharing a canonical element. For instance, if
     multiple compilation units define aliases to the same canonical symbol,
     that symbol will be a canonical element in both of the units' alias
     trackers, and thus their [aliases_of_canonical_names] will have a key in
     common. *)
  let merge_aliases _canonical aliases1 aliases2 =
    match Aliases_of_canonical_element.union aliases1 aliases2 with
    | Ok aliases -> Some aliases
    | Bottom ->
      (* CR lmaurer: This is actually possible, if probably exceedingly rare,
         so we should be returting Bottom in this case. *)
      Misc.fatal_errorf "Conflicting aliases:@ %a@ vs.@ %a"
        print t1
        print t2
  in
  let aliases_of_canonical_names =
    Name.Map.union merge_aliases
      t1.aliases_of_canonical_names
      t2.aliases_of_canonical_names
  in
  let aliases_of_consts =
    Const.Map.union merge_aliases
      t1.aliases_of_consts
      t2.aliases_of_consts
  in
  let symbol_data =
    Binding_time.With_name_mode.create
      Binding_time.symbols
      Name_mode.normal
  in
  let binding_times_and_modes =
    Name.Map.union (fun name data1 data2 ->
        Name.pattern_match name
          ~var:(fun var ->
            (* TODO: filter variables on export and restore fatal_error *)
            if Binding_time.(equal (With_name_mode.binding_time data1)
                               imported_variables)
            then Some data2
            else if Binding_time.(equal (With_name_mode.binding_time data2)
                               imported_variables)
            then Some data1
            else
              Misc.fatal_errorf
                "Variable %a is present in multiple environments"
                Variable.print var)
          ~symbol:(fun _sym ->
            assert (Binding_time.With_name_mode.equal data1 symbol_data);
            assert (Binding_time.With_name_mode.equal data2 symbol_data);
            Some data1))
      t1.binding_times_and_modes
      t2.binding_times_and_modes
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }

let get_canonical_ignoring_name_mode t name =
  let elt = Simple.name name in
  match canonical t elt with
  | Is_canonical ->
    elt
  | Alias_of_canonical { canonical_element; coercion_to_canonical; } ->
    let coercion_from_canonical = Coercion.inverse coercion_to_canonical in
    Simple.apply_coercion_exn canonical_element coercion_from_canonical

let clean_for_export
      { canonical_elements;
        aliases_of_canonical_names;
        aliases_of_consts;
        binding_times_and_modes; } =
  let binding_times_and_modes =
    Name.Map.mapi (fun name binding_time_and_mode ->
        let module BTM = Binding_time.With_name_mode in
        let new_mode =
          if Name.is_var name then Name_mode.in_types
          else BTM.name_mode binding_time_and_mode
        in
        BTM.create (BTM.binding_time binding_time_and_mode) new_mode)
      binding_times_and_modes
  in
  let aliases_of_canonical_names =
    (* Note: the relative order of the aliases and of their canonical element
       will be unchanged, as it only depends on the binding times.
    *)
    Name.Map.map Aliases_of_canonical_element.move_variables_to_mode_in_types
      aliases_of_canonical_names
  in
  let aliases_of_consts =
    Const.Map.map Aliases_of_canonical_element.move_variables_to_mode_in_types
      aliases_of_consts
  in
  { canonical_elements;
    aliases_of_canonical_names;
    aliases_of_consts;
    binding_times_and_modes;
  }
