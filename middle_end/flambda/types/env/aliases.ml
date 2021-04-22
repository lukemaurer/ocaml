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

type coercion_to_canonical = Coercion.t
type map_to_canonical = coercion_to_canonical Simple.Map.t

let compose_map_values_exn map ~then_:coercion =
  if Coercion.is_id coercion then map else
    Simple.Map.map (fun old_coercion ->
      Coercion.compose_exn old_coercion ~then_:coercion
    ) map

let fatal_inconsistent ~func_name elt coercion1 coercion2 =
  Misc.fatal_errorf "[%s] maps with inconsistent  element/coercion couples; \
                     %a has coercions %a and %a"
    func_name
    Simple.print elt
    Coercion.print coercion1
    Coercion.print coercion2

let map_inter map1 map2 =
  Simple.Map.merge (fun elt coercion1 coercion2 ->
    match coercion1, coercion2 with
    | None, None | Some _, None | None, Some _ -> None
    | Some coercion1, Some coercion2 ->
      if Coercion.equal coercion1 coercion2 then
        Some coercion1
      else
        fatal_inconsistent ~func_name:"Aliases.map_inter" elt coercion1 coercion2)
    map1
    map2

let map_union map1 map2 =
  Simple.Map.union (fun elt coercion1 coercion2 ->
    match coercion1, coercion2 with
    | coercion1, coercion2 ->
    if Coercion.equal coercion1 coercion2 then
      Some coercion1
    else
      fatal_inconsistent ~func_name:"Aliases.map_union" elt coercion1 coercion2)
    map1
    map2

module Aliases_of_canonical_element : sig
  type t

  val print : Format.formatter -> t -> unit

  val invariant : t -> unit

  val empty : t
  val is_empty : t -> bool

  val add : t -> Simple.t -> coercion_to_canonical:Coercion.t -> Name_mode.t -> t

  val find_earliest_candidates
    : t
    -> min_name_mode:Name_mode.t
    -> map_to_canonical option

  val all : t -> map_to_canonical

  val mem : t -> Simple.t -> bool

  val union : t -> t -> t
  val inter : t -> t -> t

  val import : (Simple.t -> Simple.t) -> t -> t

  val merge : t -> t -> t

  val move_variables_to_mode_in_types : t -> t

  val compose : t -> then_:Coercion.t -> t
end = struct
  type t = {
    aliases : map_to_canonical Name_mode.Map.t;
    all : map_to_canonical;
  }

  let invariant { aliases; all; } =
    (* The elements in [aliases] have disjoint set of keys. *)
    let aliases_union : map_to_canonical =
      Name_mode.Map.fold (fun _name_mode map acc ->
        Simple.Map.union (fun elt _coercion1 _coercion2 ->
          Misc.fatal_errorf "[Aliases_of_canonical_element.invariant]: \
                             element %a appears in several modes"
            Simple.print elt)
          map
          acc)
        aliases
        Simple.Map.empty
    in
    (* [all] is the union of all elements in [aliases] *)
    if Simple.Map.equal Coercion.equal all aliases_union then
      ()
    else
      Misc.fatal_errorf "[Aliases_of_canonical_element.invariant]: \
                         [aliases] and [all] are not consistent"

  let print ppf { aliases; all = _; } =
    Name_mode.Map.print (Simple.Map.print Coercion.print) ppf aliases

  let empty = {
    aliases = Name_mode.Map.empty;
    all = Simple.Map.empty;
  }

  let is_empty t = Simple.Map.is_empty t.all

  let add t elt ~coercion_to_canonical name_mode =
    if Simple.Map.mem elt t.all then begin
      Misc.fatal_errorf "%a already added to [Aliases_of_canonical_element]: \
                         %a"
        Simple.print elt
        print t
    end;
    let aliases =
      Name_mode.Map.update name_mode
        (function
          | None -> Some (Simple.Map.singleton elt coercion_to_canonical)
          | Some elts ->
            if !Clflags.flambda_invariant_checks then begin
              assert (not (Simple.Map.mem elt elts))
            end;
            Some (Simple.Map.add elt coercion_to_canonical elts))
        t.aliases
    in
    let all = Simple.Map.add elt coercion_to_canonical t.all in
    { aliases;
      all;
    }

  let find_earliest_candidates t ~min_name_mode =
    Name_mode.Map.fold (fun order aliases res_opt ->
      match res_opt with
      | Some _ -> res_opt
      | None ->
        begin match
          Name_mode.compare_partial_order
            order min_name_mode
        with
        | None -> None
        | Some result ->
          if result >= 0 then Some aliases else None
        end)
    t.aliases
    None

  let mem t elt =
    Simple.Map.mem elt t.all

  let all t = t.all

  let union t1 t2 =
    let aliases : map_to_canonical Name_mode.Map.t=
      Name_mode.Map.union (fun _order elts1 elts2 ->
        Some (map_union elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = map_union t1.all t2.all;
      }
    in
    invariant t; (* CR xclerc for xclerc: not guaranteed to hold *)
    t

  let inter t1 t2 =
    let aliases =
      Name_mode.Map.merge (fun _order elts1 elts2 ->
        match elts1, elts2 with
        | None, None | Some _, None | None, Some _ -> None
        | Some elts1, Some elts2 ->
          Some (map_inter elts1 elts2))
        t1.aliases t2.aliases
    in
    let t =
      { aliases;
        all = map_inter t1.all t2.all;
      }
    in
    invariant t;
    t

  let import import_simple { aliases; all } =
    let map_simple elts =
      Simple.Map.fold (fun elt coercion acc ->
        Simple.Map.add (import_simple elt) coercion acc)
        elts
        Simple.Map.empty
    in
    let aliases = Name_mode.Map.map map_simple aliases in
    let all = map_simple all in
    let t = { aliases; all } in
    invariant t; (* CR xclerc for xclerc: not guaranteed to hold *)
    t

  let merge t1 t2 =
    let aliases =
      Name_mode.Map.union (fun _mode map1 map2 ->
        Some (map_union map1 map2)
      )
        t1.aliases
        t2.aliases
    in
    let all = map_union t1.all t2.all in
    let t = { aliases; all; } in
    invariant t; (* CR xclerc for xclerc: not guaranteed to hold *)
    t

  let compose { aliases; all; } ~then_ =
    let f m =
      Simple.Map.map (Coercion.compose_exn ~then_) m
    in
    let aliases = Name_mode.Map.map f aliases in
    let all = f all in
    { aliases; all; }

  let move_variables_to_mode_in_types { aliases; all; } =
    let (no_vars_aliases, all_variables) =
      Name_mode.Map.fold (fun mode aliases (no_vars_aliases, all_variables) ->
          let key_is_var key _ = Simple.is_var key in
          let (vars, non_vars) = Simple.Map.partition key_is_var aliases in
          let no_vars_aliases =
            if Simple.Map.is_empty non_vars then no_vars_aliases
            else Name_mode.Map.add mode non_vars no_vars_aliases
          in
          no_vars_aliases, map_union vars all_variables)
        aliases
        (Name_mode.Map.empty, Simple.Map.empty)
    in
    let aliases =
      if Name_mode.Map.mem Name_mode.in_types no_vars_aliases
      then Misc.fatal_errorf "move_variables_to_mode_in_types: \
             The following non-vars have mode In_types:@ %a"
             (Simple.Map.print Coercion.print)
             (Name_mode.Map.find Name_mode.in_types no_vars_aliases)
      else
        if Simple.Map.is_empty all_variables then no_vars_aliases
        else Name_mode.Map.add Name_mode.in_types all_variables no_vars_aliases
    in
    { aliases; all; }
end

type t = {
  canonical_elements : (Simple.t * coercion_to_canonical) Simple.Map.t;
  (* Canonical elements that have no known aliases are not included in
     [canonical_elements]. *)
  aliases_of_canonical_elements : Aliases_of_canonical_element.t Simple.Map.t;
  (* For [elt |-> aliases] in [aliases_of_canonical_elements], then
     [aliases] never includes [elt]. *)
  (* CR mshinwell: check this always holds *)
  binding_times_and_modes : Binding_time.With_name_mode.t Simple.Map.t;
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

let print ppf { canonical_elements; aliases_of_canonical_elements;
                binding_times_and_modes; } =
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
     @[<hov 1>(aliases_of_canonical_elements@ %a)@]@ \
     @[<hov 1>(binding_times_and_modes@ %a)@]\
     )@]"
    (Simple.Map.print print_element_and_coercion) canonical_elements
    (Simple.Map.print Aliases_of_canonical_element.print)
    aliases_of_canonical_elements
    (Simple.Map.print Binding_time.With_name_mode.print)
    binding_times_and_modes

let defined_earlier t alias ~than =
  let alias = Simple.without_coercion alias in
  let than = Simple.without_coercion than in
  let info1 = Simple.Map.find alias t.binding_times_and_modes in
  let info2 = Simple.Map.find than t.binding_times_and_modes in
  Binding_time.strictly_earlier
    (Binding_time.With_name_mode.binding_time info1)
    ~than:(Binding_time.With_name_mode.binding_time info2)

let name_mode t elt =
  Binding_time.With_name_mode.name_mode
    (Simple.Map.find elt t.binding_times_and_modes)

let invariant t =
  if !Clflags.flambda_invariant_checks then begin
    let _all_aliases : map_to_canonical =
      Simple.Map.fold (fun canonical_element aliases all_aliases ->
          Aliases_of_canonical_element.invariant aliases;
          let aliases = Aliases_of_canonical_element.all aliases in
          if not (Simple.Map.for_all (fun elt _coercion ->
            defined_earlier t canonical_element ~than:elt) aliases)
          then begin
            Misc.fatal_errorf "Canonical element %a is not earlier than \
                all of its aliases:@ %a"
              Simple.print canonical_element
              print t
          end;
          if Simple.Map.mem canonical_element aliases then begin
            Misc.fatal_errorf "Canonical element %a occurs in alias set:@ %a"
              Simple.print canonical_element
              (Simple.Map.print Coercion.print) aliases
          end;
          if not (Simple.Map.is_empty (map_inter aliases all_aliases)) then
          begin
            Misc.fatal_errorf "Overlapping alias sets:@ %a" print t
          end;
          map_union aliases all_aliases)
        t.aliases_of_canonical_elements
        Simple.Map.empty
    in
    ()
  end

let empty = {
  (* CR mshinwell: Rename canonical_elements, maybe to
     aliases_to_canonical_elements. *)
  canonical_elements = Simple.Map.empty;
  aliases_of_canonical_elements = Simple.Map.empty;
  binding_times_and_modes = Simple.Map.empty;
}

type canonical =
  | Is_canonical
  | Alias_of_canonical of {
      canonical_element : Simple.t;
      coercion_to_canonical : coercion_to_canonical;
    }

let canonical t element : canonical =
  match Simple.Map.find element t.canonical_elements with
  | exception Not_found -> Is_canonical
  | canonical_element, coercion_to_canonical ->
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Simple.equal element canonical_element))
    end;
    Alias_of_canonical { canonical_element; coercion_to_canonical; }

let get_aliases_of_canonical_element t ~canonical_element =
  match Simple.Map.find canonical_element t.aliases_of_canonical_elements with
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
let add_alias_between_canonical_elements t ~canonical_element
      ~coercion_to_canonical:coercion_to_canonical ~to_be_demoted =
  if Simple.equal canonical_element to_be_demoted then begin
    if Coercion.is_id coercion_to_canonical then begin
      t
    end else
      Misc.fatal_errorf
        "Cannot add an alias of %a@ to itself with a non-identity coercion@ %a"
        Simple.print canonical_element
        Coercion.print coercion_to_canonical
  end else
    let aliases_of_to_be_demoted =
      get_aliases_of_canonical_element t ~canonical_element:to_be_demoted
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_to_be_demoted canonical_element))
    end;
    let canonical_elements =
      t.canonical_elements
      |> Simple.Map.fold (fun alias coercion_to_to_be_demoted canonical_elements ->
        let coercion_to_canonical =
          Coercion.compose_exn coercion_to_to_be_demoted ~then_:coercion_to_canonical
        in
        Simple.Map.add alias (canonical_element, coercion_to_canonical) canonical_elements)
        (Aliases_of_canonical_element.all aliases_of_to_be_demoted)
      |> Simple.Map.add to_be_demoted (canonical_element, coercion_to_canonical)
    in
    let aliases_of_canonical_element =
      get_aliases_of_canonical_element t ~canonical_element
    in
    if !Clflags.flambda_invariant_checks then begin
      assert (not (Aliases_of_canonical_element.mem
        aliases_of_canonical_element to_be_demoted));
      assert (Aliases_of_canonical_element.is_empty (
        Aliases_of_canonical_element.inter
          aliases_of_canonical_element aliases_of_to_be_demoted))
    end;
    let aliases =
      Aliases_of_canonical_element.add
        (Aliases_of_canonical_element.union
           (Aliases_of_canonical_element.compose aliases_of_to_be_demoted ~then_:coercion_to_canonical)
           aliases_of_canonical_element)
        to_be_demoted
        ~coercion_to_canonical
        (name_mode t to_be_demoted)
    in
    let aliases_of_canonical_elements =
      t.aliases_of_canonical_elements
      |> Simple.Map.remove to_be_demoted
      |> Simple.Map.add (* replace *) canonical_element aliases
    in
    let res =
    { canonical_elements;
      aliases_of_canonical_elements;
      binding_times_and_modes = t.binding_times_and_modes;
    } in
    invariant res;
    res

type to_be_demoted = Demote_element1 | Demote_element2

let choose_canonical_element_to_be_demoted t ~canonical_element1
      ~canonical_element2 =
  if defined_earlier t canonical_element1 ~than:canonical_element2
  then Demote_element2 else Demote_element1

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
      | Demote_element1 ->
        let coercion_from_canonical_element1_to_canonical_element2 =
          Coercion.inverse
            coercion_from_canonical_element2_to_canonical_element1
        in
        canonical_element2, canonical_element1, element1,
        coercion_from_canonical_element1_to_canonical_element2,
        coercion_from_element1_to_canonical_element1
      | Demote_element2 ->
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
    { t;
      canonical_element;
      demoted_alias;
    }
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
  let t =
    { t with binding_times_and_modes =
               Simple.Map.add element1 binding_time_and_mode1
                 (Simple.Map.add element2 binding_time_and_mode2
                    t.binding_times_and_modes);
    }
  in
  let add_result = add_alias t ~element1 ~coercion_from_element2_to_element1 ~element2 in
  if !Clflags.flambda_invariant_checks then begin
    invariant_add_result ~original_t add_result
  end;
  add_result

let mem t element =
  Simple.Map.mem element t.binding_times_and_modes

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
Format.eprintf "looking for canonical for %a, candidate canonical %a, min order %a\n%!"
  Simple.print element
  Simple.print canonical_element
  Name_mode.print min_name_mode;
*)
  let find_earliest () =
    (* There used to be a shortcut that avoided a consulting the aliases in the
       common case that [element] is itself canonical and has no aliases, since
       then it does not appear in [canonical_elements]. However, this shortcut
       was broken: a canonical element *with* known aliases may still not appea

       in [canonical_elements]. See tests/flambda2-aliases for a test that gave
       incorrect output (saying x/39 had no aliases). It may be worth restoring
       the shortcut, perhaps by returning more information from [canonical]. *)
    let aliases = get_aliases_of_canonical_element t ~canonical_element in
    match
      Aliases_of_canonical_element.find_earliest_candidates aliases
        ~min_name_mode
    with
    | Some at_earliest_mode ->
      (* Aliases_of_canonical_element.find_earliest_candidates only returns
         non-empty sets *)
      assert (not (Simple.Map.is_empty at_earliest_mode));
      let earliest, coercion_from_earliest_to_canonical =
        Simple.Map.fold (fun elt coercion ((min_elt, _min_coercion) as min_binding) ->
            if defined_earlier t elt ~than:min_elt
            then elt, coercion
            else min_binding)
          at_earliest_mode
          (Simple.Map.min_binding at_earliest_mode)
      in
      let coercion_from_earliest_to_element =
        Coercion.compose_exn coercion_from_earliest_to_canonical
          ~then_:coercion_from_canonical_to_element
      in
      Simple.with_coercion earliest coercion_from_earliest_to_element
    | None -> raise Not_found
  in
  match Name_mode.compare_partial_order name_mode min_name_mode with
  | None -> find_earliest ()
  | Some c ->
    if c >= 0 then
      Simple.with_coercion canonical_element coercion_from_canonical_to_element
    else find_earliest ()

let make_get_aliases_result
    ~element:_
    ~canonical_element
    ~coercion_from_canonical_to_element
    ~(names_with_coercions_to_element : Coercion.t Simple.Map.t) =
  Simple.Set.add
    (Simple.with_coercion canonical_element coercion_from_canonical_to_element)
    (Simple.Map.fold (fun simple coercion set ->
        Simple.Set.add (Simple.with_coercion simple coercion) set
      ) names_with_coercions_to_element Simple.Set.empty)

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
    let aliases_with_coercions_to_canonical =
      Aliases_of_canonical_element.all
        (get_aliases_of_canonical_element t ~canonical_element)
    in
    let coercion_from_canonical_to_element =
      Coercion.inverse coercion_from_element_to_canonical
    in
    (* If any composition fails, then our coercions are inconsistent somehow,
       which should only happen when meeting *)
    let names_with_coercions_to_element =
      compose_map_values_exn aliases_with_coercions_to_canonical
        ~then_:coercion_from_canonical_to_element
    in
    
    if !Clflags.flambda_invariant_checks then begin
      let element_coerced_to_canonical =
        Simple.apply_coercion_exn element coercion_from_element_to_canonical
      in
      (* These aliases are all equivalent to the canonical element, and so is
         our original [element] if we coerce it first, so the coerced form of
         [element] should be among the aliases. *)
      assert (Simple.Map.exists 
        (fun name coercion_from_name_to_canonical ->
          let name_coerced_to_canonical =
            Simple.apply_coercion_exn
              name
              coercion_from_name_to_canonical
          in
          Simple.equal element_coerced_to_canonical name_coerced_to_canonical
        ) aliases_with_coercions_to_canonical)
    end;
    make_get_aliases_result
      ~element
      ~canonical_element
      ~coercion_from_canonical_to_element
      ~names_with_coercions_to_element

let all_ids_for_export { canonical_elements = _;
                         aliases_of_canonical_elements = _;
                         binding_times_and_modes; } =
  Simple.Map.fold (fun elt _binding_time_and_mode ids ->
    Ids_for_export.add_simple ids elt)
    binding_times_and_modes
    Ids_for_export.empty

let import import_map { canonical_elements;
                        aliases_of_canonical_elements;
                        binding_times_and_modes; } =
  let import_simple x = Ids_for_export.Import_map.simple import_map x in
  let canonical_elements =
    Simple.Map.fold (fun elt (canonical, coercion) acc ->
      Simple.Map.add (import_simple elt) (import_simple canonical, coercion) acc)
      canonical_elements
      Simple.Map.empty
  in
  let aliases_of_canonical_elements =
    (* Warning: we assume that the aliases in the two alias trackers are disjoint

       but nothing stops them from sharing a canonical element. For instance, if
       multiple compilation units define aliases to the same canonical symbol,
       that symbol will be a canonical element in both of the units' alias
       trackers, and thus their [aliases_of_canonical_names] will have a key in
       common. *)
    Simple.Map.fold (fun canonical aliases acc ->
        Simple.Map.add (import_simple canonical)
          (Aliases_of_canonical_element.import import_simple aliases)
          acc)
      aliases_of_canonical_elements
      Simple.Map.empty
  in
  let binding_times_and_modes =
    Simple.Map.fold (fun simple binding_time_and_mode acc ->
        Simple.Map.add (import_simple simple) binding_time_and_mode acc)
      binding_times_and_modes
      Simple.Map.empty
  in
  { canonical_elements;
    aliases_of_canonical_elements;
    binding_times_and_modes;
  }

let merge t1 t2 =
  let canonical_elements =
    Simple.Map.disjoint_union
      t1.canonical_elements
      t2.canonical_elements
  in
  let aliases_of_canonical_elements =
    (* Warning: here the keys of the map can come from other
       compilation units, so we cannot assume the keys are disjoint *)
    Simple.Map.union (fun _simple aliases1 aliases2 ->
        Some (Aliases_of_canonical_element.merge aliases1 aliases2))
      t1.aliases_of_canonical_elements
      t2.aliases_of_canonical_elements
  in
  let symbol_data =
    Binding_time.With_name_mode.create
      Binding_time.symbols
      Name_mode.normal
  in
  let binding_times_and_modes =
    Simple.Map.union (fun simple data1 data2 ->
        Simple.pattern_match simple
          ~const:(fun _ ->
            assert (Binding_time.With_name_mode.equal data1 data2);
            Some data1)
          ~name:(fun name ->
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
                Some data1)))
      t1.binding_times_and_modes
      t2.binding_times_and_modes
  in
  { canonical_elements;
    aliases_of_canonical_elements;
    binding_times_and_modes;
  }

let get_canonical_ignoring_name_mode t name =
  let elt = Simple.name name in
  match canonical t elt with
  | Is_canonical ->
    elt
  | Alias_of_canonical { canonical_element; coercion_to_canonical } ->
    let coercion_from_canonical = Coercion.inverse coercion_to_canonical in
    Simple.apply_coercion_exn canonical_element coercion_from_canonical

let clean_for_export
      { canonical_elements;
        aliases_of_canonical_elements;
        binding_times_and_modes; } =
  let binding_times_and_modes =
    Simple.Map.mapi (fun simple binding_time_and_mode ->
        let module BTM = Binding_time.With_name_mode in
        let new_mode =
          if Simple.is_var simple then Name_mode.in_types
          else BTM.name_mode binding_time_and_mode
        in
        BTM.create (BTM.binding_time binding_time_and_mode) new_mode)
      binding_times_and_modes
  in
  let aliases_of_canonical_elements =
    (* Note: the relative order of the aliases and of their canonical element
       will be unchanged, as it only depends on the binding times.
    *)
    Simple.Map.map Aliases_of_canonical_element.move_variables_to_mode_in_types
      aliases_of_canonical_elements
  in
  { canonical_elements;
    aliases_of_canonical_elements;
    binding_times_and_modes;
  }
