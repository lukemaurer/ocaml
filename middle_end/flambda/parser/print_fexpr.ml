[@@@ocaml.warning "+a-4-30-40-41-42"]

(* CR lmaurer: Need to adjust to new syntax once that's more settled. (Also
 * need to update error messages once the syntax is settled.) *)

open! Fexpr

let pp_list ~sep f ppf =
  Format.pp_print_list f
    ~pp_sep:(fun ppf () -> Format.fprintf ppf sep)
    ppf

let pp_star_list f = pp_list ~sep:" *@ " f

let pp_comma_list f = pp_list ~sep:",@ " f

let pp_semi_list f = pp_list ~sep:";@ " f

let empty_fmt : (unit, Format.formatter, unit) format = ""

let pp_with ?(prefix=empty_fmt) ?(suffix=empty_fmt) ppf =
  Format.kdprintf (fun pp ->
    Format.fprintf ppf prefix;
    pp ppf;
    Format.fprintf ppf suffix)

let pp_option ?prefix ?suffix f ppf = function
  | None -> ()
  | Some x -> pp_with ?prefix ?suffix ppf "%a" f x

let recursive ppf = function
  | Nonrecursive -> ()
  | Recursive -> Format.fprintf ppf "@ rec"

let continuation_sort ppf sort =
  Format.pp_print_string ppf
  @@
  match sort with
  | Normal -> "normal"
  | Exn -> "exn"
  | Define_root_symbol -> "define_root_symbol"

let char_between c (low, high) =
  Char.compare low c <= 0 && Char.compare c high <= 0

let is_identstart c =
  Char.equal c '_' || char_between c ('a', 'z') || char_between c ('A', 'Z')

let is_identchar c =
  is_identstart c || Char.equal c '\'' || char_between c ('0', '9')

let is_unquoted_symbol s =
  not (String.equal s "") && Misc.Stdlib.String.for_all is_identchar s

let is_unquoted_ident s =
  not (String.equal s "")
  && is_identstart s.[0]
  && Misc.Stdlib.String.for_all is_identchar s

let symbol_part ppf s =
  if is_unquoted_symbol s
    then Format.pp_print_string ppf s
    else Format.fprintf ppf "`%s`" s

let symbol ppf { txt = cunit, s; loc = _ } =
  Format.pp_print_char ppf '$';
  cunit |> Option.iter (fun { ident; linkage_name } ->
    symbol_part ppf ident;
    linkage_name |> Option.iter (fun linkage_name ->
      Format.fprintf ppf "/%a" symbol_part linkage_name
    );
    Format.pp_print_char ppf '.'
  );
  symbol_part ppf s

let ident ppf s =
  if is_unquoted_ident s && not (Flambda_lex.is_keyword s)
    then Format.pp_print_string ppf s
    else Format.fprintf ppf "`%s`" s

let variable ppf { txt = s; loc = _ } =
  ident ppf s

let var_within_closure ppf { txt = s; loc = _ } =
  ident ppf s

let code_id ppf ({ txt = s; loc = _ } : code_id) =
  ident ppf s

let closure_id ppf ({ txt = s; loc = _ } : closure_id) =
  ident ppf s

let continuation_id ppf ({ txt = s; loc = _ } : continuation_id) =
  ident ppf s

let special_continuation ppf special_cont =
  match special_cont with
  | Done -> Format.fprintf ppf "done"
  | Error -> Format.fprintf ppf "error"

let continuation ppf cont =
  match cont with
  | Named id -> continuation_id ppf id
  | Special special_cont -> special_continuation ppf special_cont

let result_continuation ppf rcont =
  match rcont with
  | Return c -> continuation ppf c
  | Never_returns -> Format.fprintf ppf "never"

let exn_continuation ppf c =
  Format.fprintf ppf "* %a" continuation c

let naked_number_kind ppf (nnk : naked_number_kind) =
  Format.pp_print_string ppf
  @@
  match nnk with
  | Naked_immediate -> "imm"
  | Naked_float -> "float"
  | Naked_int32 -> "int32"
  | Naked_int64 -> "int64"
  | Naked_nativeint -> "nativeint"

let kind ppf (k : kind) =
  match k with
  | Value -> Format.pp_print_string ppf "val"
  | Naked_number nnk -> naked_number_kind ppf nnk
  | Fabricated -> Format.pp_print_string ppf "fabricated"

let kind_with_subkind ppf (k : kind_with_subkind) =
  match k with
  | Naked_number nnk -> naked_number_kind ppf nnk
  | _ ->
    Format.pp_print_string ppf
    @@
    match k with
    | Any_value -> "val"
    | Naked_number _ -> assert false
    | Boxed_float -> "float boxed"
    | Boxed_int32 -> "int32 boxed"
    | Boxed_int64 -> "int64 boxed"
    | Boxed_nativeint -> "nativeint boxed"
    | Tagged_immediate -> "imm tagged"

let arity ppf (a : arity) =
  match a with
  | [] -> Format.pp_print_string ppf "unit"
  | _ -> pp_star_list kind_with_subkind ppf a

let kinded_variable ppf (v, (k:kind_with_subkind option)) =
  match k with
  | None ->
    variable ppf v
  | Some k ->
    Format.fprintf ppf "@[<2>%a :@ %a@]" variable v kind_with_subkind k

let standard_int ?prefix ?suffix () ppf (i : standard_int) =
  let str = match i with
    | Tagged_immediate -> None
    | Naked_immediate -> Some "imm"
    | Naked_int32 -> Some "int32"
    | Naked_int64 -> Some "int64"
    | Naked_nativeint -> Some "nativeint"
  in
  pp_option ?prefix ?suffix Format.pp_print_string ppf str

let convertible_type ppf (t : standard_int_or_float) =
  let str = match t with
    | Tagged_immediate -> "imm tagged"
    | Naked_immediate -> "imm"
    | Naked_float -> "float"
    | Naked_int32 -> "int32"
    | Naked_int64 -> "int64"
    | Naked_nativeint -> "nativeint"
  in
  Format.pp_print_string ppf str

let signed_or_unsigned ?prefix ?suffix () ppf (s : signed_or_unsigned) =
  match s with
  | Signed -> ()
  | Unsigned -> pp_with ?prefix ?suffix ppf "unsigned"

let field_of_block ppf : field_of_block -> unit = function
  | Symbol s -> symbol ppf s
  | Dynamically_computed v -> variable ppf v
  | Tagged_immediate i -> Format.fprintf ppf "%s" i

let float ppf f = Format.fprintf ppf "%h" f

let const ppf (c:Fexpr.const) = match c with
  | Naked_immediate i -> Format.fprintf ppf "%si" i
  | Tagged_immediate i -> Format.fprintf ppf "%s" i
  | Naked_float f -> float ppf f
  | Naked_int32 i -> Format.fprintf ppf "%lil" i
  | Naked_int64 i -> Format.fprintf ppf "%LiL" i
  | Naked_nativeint i -> Format.fprintf ppf "%Lin" i

let simple ppf : simple -> unit = function
  | Symbol s -> symbol ppf s
  | Var v -> variable ppf v
  | Const c -> const ppf c

let simple_args ~omit_if_empty ppf = function
  | [] when omit_if_empty -> ()
  | args ->
    Format.fprintf ppf "@ (@[<hv>%a@])"
      (pp_comma_list simple) args

let name ppf : name -> unit = function
  | Symbol s -> symbol ppf s
  | Var v -> variable ppf v

let mutability ?prefix ?suffix () ppf mut =
  let str =
    match mut with
    | Mutable -> Some "mutable"
    | Immutable -> None
    | Immutable_unique -> Some "immutable_unique"
  in
  pp_option ?prefix ?suffix Format.pp_print_string ppf str

let array_kind ?prefix ?suffix () ppf (ak : array_kind) =
  let str =
    match ak with
    | Immediates -> None
    | Values -> Some "val"
    | Naked_floats -> Some "float"
    | Float_array_opt_dynamic -> Some "dynamic"
  in
  pp_option ?prefix ?suffix Format.pp_print_string ppf str

let init_or_assign ppf ia =
  let str =
    match ia with
    | Initialization -> "="
    | Assignment -> "<-"
  in
  Format.fprintf ppf "%s" str

let boxed_variable ppf var ~kind =
  Format.fprintf ppf "%a : %s boxed" variable var kind

let float_or_variable ppf : float or_variable -> unit = function
  | Const f -> float ppf f
  | Var v -> variable ppf v

let static_data ppf : static_data -> unit = function
  | Block { tag; mutability = mut; elements = elts } ->
    Format.fprintf ppf "Block %a%i (@[<hv>%a@])"
      (mutability ~suffix:"@ " ()) mut
      tag
      (pp_comma_list field_of_block) elts
  | Boxed_float (Const f) -> Format.fprintf ppf "%h" f
  | Boxed_int32 (Const i) -> Format.fprintf ppf "%lil" i
  | Boxed_int64 (Const i) -> Format.fprintf ppf "%LiL" i
  | Boxed_nativeint (Const i) -> Format.fprintf ppf "%Lin" i
  | Boxed_float (Var v) -> boxed_variable ppf v ~kind:"float"
  | Boxed_int32 (Var v) -> boxed_variable ppf v ~kind:"int32"
  | Boxed_int64 (Var v) -> boxed_variable ppf v ~kind:"int64"
  | Boxed_nativeint (Var v) -> boxed_variable ppf v ~kind:"nativeint"
  | Immutable_float_block elements ->
    Format.fprintf ppf "Float_block (%a)"
      (pp_comma_list float_or_variable) elements
  | Immutable_float_array elements ->
    Format.fprintf ppf "Float_array [|%a|]"
      (pp_semi_list float_or_variable) elements
  | Mutable_string { initial_value = s } ->
    Format.fprintf ppf "mutable \"%s\"" (s |> String.escaped)
  | Immutable_string s ->
    Format.fprintf ppf "\"%s\"" (s |> String.escaped)

let static_data_binding ppf { symbol = s; defining_expr = sp } =
  Format.fprintf ppf "%a =@ %a"
    symbol s
    static_data sp

let invalid ppf = function
  | Halt_and_catch_fire ->
    Format.fprintf ppf "HCF"
  | Treat_as_unreachable ->
    Format.fprintf ppf "Unreachable"

let binary_int_arith_op ppf (o : binary_int_arith_op) =
  Format.pp_print_string ppf
  @@
  match o with
  | Add -> "+"
  | Sub -> "-"
  | Mul -> "*"
  | Div -> "/"
  | Mod -> "%"
  | And -> "land"
  | Or -> "lor"
  | Xor -> "lxor"

let binary_float_arith_op ppf (o : binary_float_arith_op) =
  Format.pp_print_string ppf
  @@
  match o with
  | Add -> "+."
  | Sub -> "-."
  | Mul -> "*."
  | Div -> "/."

let int_comp ppf (o : ordered_comparison comparison_behaviour) =
  Format.pp_print_string ppf
  @@
  match o with
  | Yielding_bool Lt -> "<"
  | Yielding_bool Gt -> ">"
  | Yielding_bool Le -> "<="
  | Yielding_bool Ge -> ">="
  | Yielding_int_like_compare_functions -> "?"

let float_comp ppf (o : comparison comparison_behaviour) =
  Format.pp_print_string ppf
  @@
  match o with
  | Yielding_bool Eq -> "=."
  | Yielding_bool Neq -> "!=."
  | Yielding_bool Lt -> "<."
  | Yielding_bool Gt -> ">."
  | Yielding_bool Le -> "<=."
  | Yielding_bool Ge -> ">=."
  | Yielding_int_like_compare_functions -> "?"

let infix_binop ppf (b : infix_binop) =
  match b with
  | Int_arith o -> binary_int_arith_op ppf o
  | Float_arith o -> binary_float_arith_op ppf o
  | Int_comp c -> int_comp ppf c
  | Float_comp c -> float_comp ppf c

let binop ppf binop a b =
  match binop with
  | Array_load (ak, mut) ->
    Format.fprintf ppf "@[<2>%%array_load%a%a@ %a.(%a)@]"
      (array_kind ~prefix:"@ " ()) ak
      (mutability ~prefix:"@ " ()) mut
      simple a
      simple b
  | Block_load (Values { field_kind; tag; size; }, mut) ->
    let pp_size ppf (size : Int64.t option) =
      match size with
      | None -> ()
      | Some size -> Format.fprintf ppf "@ size %Li" size
    in
    let pp_field_kind ppf (field_kind : block_access_field_kind) =
      match field_kind with
      | Any_value -> ()
      | Immediate -> Format.fprintf ppf "@ imm"
    in
    Format.fprintf ppf "@[<2>%%block_load %a%a%i%a@ (%a,@ %a)@]"
      pp_field_kind field_kind
      (mutability ~suffix:"@ " ()) mut
      tag
      pp_size size
      simple a simple b
  | Phys_equal (k, comp) ->
    let name =
      match comp with
      | Eq -> "%phys_eq"
      | Neq -> "%phys_neq"
    in
    Format.fprintf ppf "@[<2>%s%a@ (%a,@ %a)@]"
      name
      (pp_option ~prefix:"@ {" ~suffix:"}" kind) k
      simple a
      simple b
  | Infix op ->
    Format.fprintf ppf "%a %a %a"
      simple a
      infix_binop op
      simple b
  | Int_arith (i, o) ->
      Format.fprintf ppf "@[<2>%%int_arith %a%a@ %a@ %a@]"
        (standard_int ~suffix:"@ " ()) i
        simple a
        binary_int_arith_op o
        simple b
  | Int_comp (i, s, c) ->
    begin
      Format.fprintf ppf "@[<2>%%int_comp %a%a%a@ %a@ %a@]"
        (standard_int ~suffix:"@ " ()) i
        (signed_or_unsigned ~suffix:"@ " ()) s
        simple a
        int_comp c
        simple b
        ;
    end

let unop ppf u =
  let str s = Format.pp_print_string ppf s in
  let box_or_unbox verb_not_imm verb_imm (bk : box_kind) =
    let print verb obj =
      Format.fprintf ppf "%%%s_%s" verb obj
    in
    match bk with
    | Naked_float -> print verb_not_imm "float"
    | Naked_int32 -> print verb_not_imm "int32"
    | Naked_int64 -> print verb_not_imm "int64"
    | Naked_nativeint -> print verb_not_imm "nativeint"
    | Untagged_immediate -> print verb_imm "imm"
  in
  match u with
  | Array_length ak ->
    Format.fprintf ppf "@[<2>%%array_length%a@]"
      (array_kind ~prefix:"@ " ()) ak
  | Box_number bk ->
    box_or_unbox "Box" "Tag" bk
  | Get_tag ->
    str "%get_tag"
  | Is_int ->
    str "%is_int"
  | Num_conv { src; dst } ->
    Format.fprintf ppf "@[<2>%%num_conv@ (%a@ -> %a)@]"
      convertible_type src
      convertible_type dst
  | Opaque_identity ->
    str "%Opaque"
  | Project_var { project_from; var } ->
    Format.fprintf ppf "@[<2>%%project_var@ %a.%a@]"
      closure_id project_from
      var_within_closure var 
  | Select_closure { move_from; move_to } ->
    Format.fprintf ppf "@[<2>%%select_closure@ (%a@ -> %a)@]"
      closure_id move_from
      closure_id move_to
  | Unbox_number bk ->
    box_or_unbox "unbox" "untag" bk

let ternop ppf t a1 a2 a3 =
  match t with
  | Array_set (ak, ia) ->
    Format.fprintf ppf "@[<2>%%array_set%a@ %a.(%a) %a %a@]"
      (array_kind ~prefix:"@ " ()) ak
      simple a1
      simple a2
      init_or_assign ia
      simple a3

let prim ppf = function
  | Unary (u, a) ->
    Format.fprintf ppf "%a %a"
      unop u
      simple a
  | Binary (b, a1, a2) ->
    binop ppf b a1 a2
  | Ternary (t, a1, a2, a3) ->
    ternop ppf t a1 a2 a3
  | Variadic (Make_block (tag, mut), elts) ->
    Format.fprintf ppf "@[<2>%%Block %a%i%a@]"
      (mutability ~suffix:"@ " ()) mut
      tag
      (simple_args ~omit_if_empty:false) elts

let parameter ppf { param; kind = k } =
  kinded_variable ppf (param, k)

let kinded_parameters ppf = function
  | [] -> ()
  | args ->
    Format.fprintf ppf "@ (@[<hv>%a@])"
      (pp_comma_list parameter) args

let apply_cont ppf (ac : Fexpr.apply_cont) =
  match ac with
  | { cont; trap_action = None; args } ->
    Format.fprintf ppf "@[<hv2>%a%a@]"
      continuation cont
      (simple_args ~omit_if_empty:true) args
  | _ ->
      failwith "TODO Apply_cont"

let switch_case ppf (v, c) =
  Format.fprintf ppf "@;| %i -> %a"
    v
    apply_cont c

let closure_elements ppf = function
  | None -> ()
  | Some ces ->
    Format.fprintf ppf "@ @[<hv2>with {";
    pp_list ~sep:";" (fun ppf ({ var; value } : closure_element) ->
      Format.fprintf ppf "@ @[<hv2>%a =@ %a@]"
        var_within_closure var
        simple value
    ) ppf ces;
    Format.fprintf ppf "@;<1 -2>}@]"

let fun_decl ppf (decl : fun_decl) =
  Format.fprintf ppf "@[<2>closure@ %t%a%a@]"
    (fun ppf -> if decl.is_tupled then Format.fprintf ppf "tupled@ ")
    code_id decl.code_id
    (pp_option ~prefix:"@ @@" closure_id) decl.closure_id

let named ppf = function
  | (Simple s : named) ->
    simple ppf s
  | Prim p ->
    prim ppf p
  | (Closure decl : named) ->
    fun_decl ppf decl

let static_closure_binding ppf (scb : static_closure_binding) =
  Format.fprintf ppf "%a =@ %a"
    symbol scb.symbol
    fun_decl scb.fun_decl

let call_kind ?prefix ?suffix () ppf ck =
  match ck with
  | Function Indirect -> ()
  | Function (Direct { code_id = c; closure_id = cl }) ->
    pp_with ?prefix ?suffix ppf "@[direct(%a%a)@]"
      code_id c
      (pp_option ~prefix:"@ @@" closure_id) cl
  | C_call { alloc } ->
    pp_with ?prefix ppf "ccall";
    if not alloc then Format.fprintf ppf "@ noalloc";
    pp_with ?suffix ppf ""

let inline_attribute ?prefix ?suffix () ppf (i : Inline_attribute.t) =
  let str =
    match i with
    | Always_inline -> Some "inline(always)"
    | Hint_inline -> Some "inline(hint)"
    | Never_inline -> Some "inline(never)"
    | Unroll i -> Some (Format.sprintf "unroll(%d)" i)
    | Default_inline -> None
  in
  pp_option ?prefix ?suffix Format.pp_print_string ppf str

let inline_attribute_opt ?prefix ?suffix () ppf i =
  pp_option ?prefix ?suffix (inline_attribute ()) ppf i

let func_name_with_optional_arities ppf (n, arities) =
  match arities with
  | None -> name ppf n
  | Some { params_arity; ret_arity } ->
    Format.fprintf ppf "@[<2>(%a :@ %a ->@ %a@,)@]"
      name n
      arity params_arity
      arity ret_arity

let symbol_scoping_rule ?prefix ?suffix () ppf sr =
  pp_option Format.pp_print_string ?prefix ?suffix ppf
  @@
  match sr with
  | None | Some Syntactic -> None
  | Some Dominator -> Some "dominator_scoped"


type scope =
  | Outer
  | Where_body
  | Continuation_body


let parens ~if_scope_is scope ppf f =
  if if_scope_is = scope
  then Format.fprintf ppf "(%t)" (f Outer)
  else f scope ppf


let rec expr scope ppf = function
  | Invalid inv ->
    invalid ppf inv
  | Apply_cont ac ->
    Format.fprintf ppf "@[cont %a@]" apply_cont ac
  | Let let_ ->
    parens ~if_scope_is:Where_body scope ppf (fun scope ppf ->
      let_expr scope ppf let_
    )
  | Let_cont { recursive = recu; body;
               bindings =
                 { name; params; sort; handler } :: rem_cont;
             } ->
    parens ~if_scope_is:Continuation_body scope ppf (fun _scope ppf ->
      Format.fprintf ppf
        "@[<v 2>%a@ @[<v>@[<v 2>where%a%a %a@[<hv2>%a@] =@ %a@]%a@]@]"
        (expr Where_body) body
        recursive recu
        (pp_option continuation_sort ~prefix:"@ ") sort
        continuation_id name
        kinded_parameters params
        (expr Continuation_body) handler
        andk rem_cont
    )
  | Let_cont _ ->
    Format.pp_print_string ppf "<malformed letk>"
  
  | Let_symbol l ->
    parens ~if_scope_is:Where_body scope ppf (fun scope ppf ->
      let_symbol_expr scope ppf l
    )

  | Switch { scrutinee; cases } ->
    Format.fprintf ppf "@[<v 2>switch %a%a@]"
      simple scrutinee
      (pp_list ~sep:"" switch_case) cases
      (* (fun ppf () -> if cases <> [] then Format.pp_print_cut ppf ()) () *)

  | Apply {
      call_kind = kind;
      inline = inline;
      inlining_state = inlining_state;
      continuation = ret;
      exn_continuation = ek;
      args;
      func;
      arities } ->
    let pp_inlining_state ppf () =
      pp_option (fun ppf -> Format.fprintf ppf "@ inlining_state %a" Inlining_state.print)
        ppf inlining_state
    in
    Format.fprintf ppf
      "@[<hv 2>apply@[<2>%a%a%a@]@ %a%a@ @[<hov2>->@ %a@]@ %a@]"
      (call_kind ~prefix:"@ " ()) kind
      (inline_attribute_opt ~prefix:"@ " ()) inline
      pp_inlining_state ()
      func_name_with_optional_arities (func, arities)
      (simple_args ~omit_if_empty:true) args
      result_continuation ret
      exn_continuation ek


and let_expr scope ppf : let_ -> unit = function
  | { bindings = first :: rest; body; closure_elements = ces; } ->
    Format.fprintf ppf "@[<v>@[<hv>@[<hv2>let %a =@ %a@]"
      variable first.var
      named first.defining_expr;
    List.iter (fun ({ var; defining_expr } : let_binding) ->
      Format.fprintf ppf "@ @[<hv2>and %a =@ %a@]"
        variable var
        named defining_expr;
    ) rest;
    Format.fprintf ppf "%a@ in@]@ %a@]"
      closure_elements ces
      (expr scope) body;
  | _ ->
    failwith "empty let?"


and let_symbol_expr scope ppf = function
  | { bindings; closure_elements; body; scoping_rule } ->
    Format.fprintf ppf "@[<v>@[<hv>@[<hv2>let %a%a@]@ in@]@ %a@]"
      (symbol_scoping_rule ~suffix:"@ " ()) scoping_rule
      symbol_bindings (bindings, closure_elements)
      (expr scope) body


and andk ppf l =
  let cont { name; params; sort; handler } =
    Format.fprintf ppf
      "@ @[<v 2>andwhere%a %a@[<hv2>%a@] =@ %a@]"
      (pp_option continuation_sort ~prefix:"@ ") sort
      continuation_id name
      kinded_parameters params
      (expr Continuation_body) handler
  in
  List.iter cont l

and symbol_bindings ppf (bindings, elements) =
  let first = ref true in
  let pp_and ppf () = if not !first then Format.fprintf ppf "@;<1 -2>and " in
  List.iter (fun b ->
    Format.fprintf ppf "%a%a" pp_and () symbol_binding b;
    first := false
  ) bindings;
  closure_elements ppf elements

and symbol_binding ppf (sb : symbol_binding) =
  match sb with
  | Data ss -> static_data_binding ppf ss
  | Code code -> code_binding ppf code
  | Closure clo -> static_closure_binding ppf clo
  | Set_of_closures soc ->
    Format.fprintf ppf "@[<hv>@[<hv2>set_of_closures@ ";
    (* Somewhat clumsily reuse the logic in [symbol_bindings] *)
    let closure_bindings_as_symbol_bindings =
      List.map (fun binding : Fexpr.symbol_binding -> Closure binding)
        soc.bindings
    in
    symbol_bindings ppf (closure_bindings_as_symbol_bindings, soc.elements);
    Format.fprintf ppf "@]@ end@]"

and code_binding ppf ({ recursive = rec_; inline; id; newer_version_of;
                        param_arity; ret_arity; params_and_body } : code) =
  Format.fprintf ppf "code@[<h>%a%a@] @[<hov2>%a%a"
    recursive rec_
    (inline_attribute_opt ~prefix:"@ " ()) inline
    code_id id
    (pp_option ~prefix:"@ newer_version_of(" ~suffix:")" code_id)
      newer_version_of;
  match params_and_body with
    | Deleted ->
      let pp_arity ppf =
        match param_arity with
        | None -> Format.print_string "???" (* invalid *)
        | Some ar -> arity ppf ar
      in
      let ret_arity =
        ret_arity |> Option.value ~default:([(Any_value : kind_with_subkind)])
      in
      Format.fprintf ppf "@ deleted :@ %t -> %a@]"
        pp_arity
        arity ret_arity
    | Present { params; closure_var; ret_cont; exn_cont; body } ->
      Format.fprintf ppf "%a@ %a@ -> %a@ * %a%a@] =@ %a"
        kinded_parameters params
        variable closure_var
        continuation_id ret_cont
        continuation_id exn_cont
        (pp_option ~prefix:"@ : " arity) ret_arity
        (expr Outer) body

let flambda_unit ppf ({ body } : flambda_unit) =
  Format.fprintf ppf "@[<v>@[%a@]@ @]"
    (expr Outer) body

let expect_test_spec ppf ({ before; after } : expect_test_spec) =
  Format.fprintf ppf
    "@[<v>%a===>@ %a@]"
    flambda_unit before
    flambda_unit after

let markdown_doc ppf nodes =
  Format.fprintf ppf "@[<v>";
  List.iter (fun (node : markdown_node) ->
    match node with
    | Text text ->
      Format.pp_print_string ppf text
    | Expect spec ->
      Format.fprintf ppf "```flexpect@ %a@ ```@ " expect_test_spec spec
  ) nodes;
  Format.fprintf ppf "@]"
