(* TEST
   include ocamlcommon
   include ocamlmiddleend
   * bytecode
*)

let () = Clflags.flambda_invariant_checks := true
let () = Clflags.dump_rawflambda := true
let () = Misc.Color.setup (Some Never)

let () =
  let dummy = "compilation_unit" in
  Compilation_unit.set_current
    (Compilation_unit.create
       (Ident.create_persistent dummy)
       (Linkage_name.create dummy))

let next_time : unit -> Binding_time.With_name_mode.t =
  let next = ref Binding_time.earliest_var in
  fun () ->
    let time = !next in
    next := Binding_time.succ time;
    Binding_time.With_name_mode.create time Name_mode.normal

let mk_simple name depth =
  let var = Simple.var (Variable.create name) in
  match depth with
  | 0 ->
    var
  | _ ->
    Simple.apply_coercion_exn var (Coercion.change_depth ~from:0 ~to_:depth)

let mk_coercion from to_ = Coercion.change_depth ~from ~to_

let add_alias
      ppf
      aliases
      ~element1
      ~binding_time_and_mode1
      ~coercion_from_element2_to_element1
      ~element2
      ~binding_time_and_mode2 =
  let { Aliases.t; canonical_element; alias_of_demoted_element; coercion_from_alias_of_demoted_to_canonical; } =
    Aliases.add
      aliases
      ~element1
      ~binding_time_and_mode1
      ~coercion_from_element2_to_element1
      ~element2
      ~binding_time_and_mode2
  in
  Format.fprintf ppf "[added] %a <--[%a]-- %a@."
    Simple.print canonical_element
    Coercion.print coercion_from_alias_of_demoted_to_canonical
    Simple.print alias_of_demoted_element;
  t

let test msg ~f =
  Format.printf "*** %s@." msg;
  f Format.std_formatter;
  Format.pp_print_newline Format.std_formatter ();
  Format.pp_print_newline Format.std_formatter ()

let () = test "empty" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  Aliases.print ppf aliases)

let () = test "single alias" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  (* x <--[f]-- y *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "single alias (inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_y = next_time () in
  let t_x = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  (* x <--[f]-- y
     ~>
     y <--[F]-- x *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "two aliases (independent)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  let z = mk_simple "z" 2 in
  (* x <--[f]-- y
     +
     x <--[g]-- z
     ~>
     x <--[f]-- y
     ^--[g]-- z *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 2 0)
      ~element2:z
      ~binding_time_and_mode2:t_z
  in
  Aliases.print ppf aliases)

let () = test "two aliases (simple chain)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  let z = mk_simple "z" 2 in
  (* x <--[f]-- y
     +
     y <--[g]-- z
     ~>
     x <--[f]-- y
     ^--[gf]-- z *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
   let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 2 1)
      ~element2:z
      ~binding_time_and_mode2:t_z
  in
  Aliases.print ppf aliases)

let () = test "two aliases (two inverses)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z = next_time () in
  let t_x = next_time () in
  let t_y = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  let z = mk_simple "z" 2 in
  (* x <--[f]-- y
     +
     y <--[g]-- z
     ~>
     z <--[GF]-- x
     ^--[G]-- y *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 2 1)
      ~element2:z
      ~binding_time_and_mode2:t_z
  in
  Aliases.print ppf aliases)

let () = test "two aliases (one inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z = next_time () in
  let t_x = next_time () in
  let t_y = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  let z = mk_simple "z" 2 in
  (* x <--[f]-- y
     +
     z <--[g]-- y
     ~>
     z <--[Fg]-- x
     ^--[g]-- y *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 1 2)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "two aliases (one inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  let z = mk_simple "z" 2 in
  (* x <--[f]-- y
     +
     z <--[g]-- y
     ~>
     x <--[f]-- y
     ^--[Gf]-- z *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 1 2)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  Aliases.print ppf aliases)

let () = test "three aliases (one inverse)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_z = next_time () in
  let t_t = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 0 in
  let z = mk_simple "z" 0 in
  let t = mk_simple "t" 0 in
  (* x <--[f]-- y
     +
     z <--[g]-- t
     +
     y <--[h]-- t
     ~>
     x <--[f]-- y
     ^^--[Ghf]-- z
      \--[hf]-- t *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 3 2)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 3 1)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  Aliases.print ppf aliases)

let () = test "three aliases (two inverses)" ~f:(fun ppf ->
  let aliases = Aliases.empty in
  let t_z = next_time () in
  let t_x = next_time () in
  let t_y = next_time () in
  let t_t = next_time () in
  let x = mk_simple "x" 0 in
  let y = mk_simple "y" 1 in
  let z = mk_simple "z" 2 in
  let t = mk_simple "t" 3 in
  (* x <--[f]-- y
     +
     z <--[g]-- t
     +
     y <--[h]-- t
     ~>
     z <--[FHg]-- x
     ^^--[Hg]-- y
     \--[g]-- t *)
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:x
      ~binding_time_and_mode1:t_x
      ~coercion_from_element2_to_element1:(mk_coercion 1 0)
      ~element2:y
      ~binding_time_and_mode2:t_y
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:z
      ~binding_time_and_mode1:t_z
      ~coercion_from_element2_to_element1:(mk_coercion 3 2)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  let aliases =
    add_alias
      ppf
      aliases
      ~element1:y
      ~binding_time_and_mode1:t_y
      ~coercion_from_element2_to_element1:(mk_coercion 3 1)
      ~element2:t
      ~binding_time_and_mode2:t_t
  in
  Aliases.print ppf aliases)

let () = print_endline "OK."
