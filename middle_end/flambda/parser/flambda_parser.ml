
(* This generated code requires the following version of CamlinternalMenhirLib: *)

let () =
  CamlinternalMenhirLib.StaticVersion.require_20200211

module MenhirBasics = struct
  
  exception Error
  
  type token = 
    | WITH
    | WHERE
    | VAL
    | UNREACHABLE
    | UNIT
    | TUPLED
    | SYMBOL of (
# 95 "flambda_parser.mly"
      (string)
# 22 "flambda_parser-in.ml"
  )
    | SWITCH
    | STUB
    | STAR
    | SIZE
    | SET_OF_CLOSURES
    | SEMICOLON
    | RPAREN
    | REC
    | RBRACE
    | PRIM_UNTAG_IMM
    | PRIM_TAG_IMM
    | PRIM_SELECT_CLOSURE
    | PRIM_PROJECT_VAR
    | PRIM_PHYS_NE
    | PRIM_PHYS_EQ
    | PRIM_OPAQUE
    | PRIM_IS_INT
    | PRIM_GET_TAG
    | PRIM_BLOCK_LOAD
    | PRIM_BLOCK
    | PLUSDOT
    | PLUS
    | PIPE
    | NOALLOC
    | NEWER_VERSION_OF
    | NATIVEINT
    | MUTABLE
    | MINUSGREATER
    | MINUSDOT
    | MINUS
    | LPAREN
    | LET
    | LBRACE
    | INT64
    | INT32
    | INT of (
# 72 "flambda_parser.mly"
       (string * char option)
# 62 "flambda_parser-in.ml"
  )
    | IN
    | IMMUTABLE_UNIQUE
    | IMM
    | IDENT of (
# 66 "flambda_parser.mly"
       (string)
# 70 "flambda_parser-in.ml"
  )
    | HCF
    | FLOAT_KIND
    | FLOAT of (
# 63 "flambda_parser.mly"
       (float)
# 77 "flambda_parser-in.ml"
  )
    | FABRICATED
    | EXN
    | ERROR
    | EQUAL
    | EOF
    | END
    | DOT
    | DONE
    | DIRECT
    | DELETED
    | CONT
    | COMMA
    | COLON
    | CODE
    | CLOSURE
    | CCALL
    | BLOCK
    | BIGARROW
    | AT
    | APPLY
    | ANDWHERE
    | AND
  
end

include MenhirBasics

let _eRR =
  MenhirBasics.Error

# 1 "flambda_parser.mly"
  
open Fexpr

let make_loc (startpos, endpos) = Lambda.of_raw_location {
  Location.loc_start = startpos;
  Location.loc_end = endpos;
  Location.loc_ghost = false;
}

let make_located txt (startpos, endpos) =
  let loc = make_loc (startpos, endpos) in
  { txt; loc }

let make_targetint = function
  | s, None -> Int64.of_string s
  | _, Some _ ->
    failwith "No modifier expected here"

let make_tag ~loc:_ = function
  | s, None -> int_of_string s
  | _, Some _ ->
    failwith "No modifier allowed for tags"

let make_tagged_immediate ~loc:_ = function
  | s, None -> s
  | _, _ ->
    failwith "Must be a tagged immediate"

let make_const_int (i, m) : const =
  match m with
  | None -> Tagged_immediate i
  | Some 'u' -> Naked_immediate i
  | Some 'n' -> Naked_nativeint (Int64.of_string i)
  | Some 'l' -> Naked_int32 (Int32.of_string i)
  | Some 'L' -> Naked_int64 (Int64.of_string i)
  | Some c -> failwith (Printf.sprintf "Unknown int modifier %c" c)


# 148 "flambda_parser-in.ml"

module Tables = struct
  
  include MenhirBasics
  
  let token2terminal : token -> int =
    fun _tok ->
      match _tok with
      | AND ->
          72
      | ANDWHERE ->
          71
      | APPLY ->
          70
      | AT ->
          69
      | BIGARROW ->
          68
      | BLOCK ->
          67
      | CCALL ->
          66
      | CLOSURE ->
          65
      | CODE ->
          64
      | COLON ->
          63
      | COMMA ->
          62
      | CONT ->
          61
      | DELETED ->
          60
      | DIRECT ->
          59
      | DONE ->
          58
      | DOT ->
          57
      | END ->
          56
      | EOF ->
          55
      | EQUAL ->
          54
      | ERROR ->
          53
      | EXN ->
          52
      | FABRICATED ->
          51
      | FLOAT _ ->
          50
      | FLOAT_KIND ->
          49
      | HCF ->
          48
      | IDENT _ ->
          47
      | IMM ->
          46
      | IMMUTABLE_UNIQUE ->
          45
      | IN ->
          44
      | INT _ ->
          43
      | INT32 ->
          42
      | INT64 ->
          41
      | LBRACE ->
          40
      | LET ->
          39
      | LPAREN ->
          38
      | MINUS ->
          37
      | MINUSDOT ->
          36
      | MINUSGREATER ->
          35
      | MUTABLE ->
          34
      | NATIVEINT ->
          33
      | NEWER_VERSION_OF ->
          32
      | NOALLOC ->
          31
      | PIPE ->
          30
      | PLUS ->
          29
      | PLUSDOT ->
          28
      | PRIM_BLOCK ->
          27
      | PRIM_BLOCK_LOAD ->
          26
      | PRIM_GET_TAG ->
          25
      | PRIM_IS_INT ->
          24
      | PRIM_OPAQUE ->
          23
      | PRIM_PHYS_EQ ->
          22
      | PRIM_PHYS_NE ->
          21
      | PRIM_PROJECT_VAR ->
          20
      | PRIM_SELECT_CLOSURE ->
          19
      | PRIM_TAG_IMM ->
          18
      | PRIM_UNTAG_IMM ->
          17
      | RBRACE ->
          16
      | REC ->
          15
      | RPAREN ->
          14
      | SEMICOLON ->
          13
      | SET_OF_CLOSURES ->
          12
      | SIZE ->
          11
      | STAR ->
          10
      | STUB ->
          9
      | SWITCH ->
          8
      | SYMBOL _ ->
          7
      | TUPLED ->
          6
      | UNIT ->
          5
      | UNREACHABLE ->
          4
      | VAL ->
          3
      | WHERE ->
          2
      | WITH ->
          1
  
  and error_terminal =
    0
  
  and token2value : token -> Obj.t =
    fun _tok ->
      match _tok with
      | AND ->
          Obj.repr ()
      | ANDWHERE ->
          Obj.repr ()
      | APPLY ->
          Obj.repr ()
      | AT ->
          Obj.repr ()
      | BIGARROW ->
          Obj.repr ()
      | BLOCK ->
          Obj.repr ()
      | CCALL ->
          Obj.repr ()
      | CLOSURE ->
          Obj.repr ()
      | CODE ->
          Obj.repr ()
      | COLON ->
          Obj.repr ()
      | COMMA ->
          Obj.repr ()
      | CONT ->
          Obj.repr ()
      | DELETED ->
          Obj.repr ()
      | DIRECT ->
          Obj.repr ()
      | DONE ->
          Obj.repr ()
      | DOT ->
          Obj.repr ()
      | END ->
          Obj.repr ()
      | EOF ->
          Obj.repr ()
      | EQUAL ->
          Obj.repr ()
      | ERROR ->
          Obj.repr ()
      | EXN ->
          Obj.repr ()
      | FABRICATED ->
          Obj.repr ()
      | FLOAT _v ->
          Obj.repr _v
      | FLOAT_KIND ->
          Obj.repr ()
      | HCF ->
          Obj.repr ()
      | IDENT _v ->
          Obj.repr _v
      | IMM ->
          Obj.repr ()
      | IMMUTABLE_UNIQUE ->
          Obj.repr ()
      | IN ->
          Obj.repr ()
      | INT _v ->
          Obj.repr _v
      | INT32 ->
          Obj.repr ()
      | INT64 ->
          Obj.repr ()
      | LBRACE ->
          Obj.repr ()
      | LET ->
          Obj.repr ()
      | LPAREN ->
          Obj.repr ()
      | MINUS ->
          Obj.repr ()
      | MINUSDOT ->
          Obj.repr ()
      | MINUSGREATER ->
          Obj.repr ()
      | MUTABLE ->
          Obj.repr ()
      | NATIVEINT ->
          Obj.repr ()
      | NEWER_VERSION_OF ->
          Obj.repr ()
      | NOALLOC ->
          Obj.repr ()
      | PIPE ->
          Obj.repr ()
      | PLUS ->
          Obj.repr ()
      | PLUSDOT ->
          Obj.repr ()
      | PRIM_BLOCK ->
          Obj.repr ()
      | PRIM_BLOCK_LOAD ->
          Obj.repr ()
      | PRIM_GET_TAG ->
          Obj.repr ()
      | PRIM_IS_INT ->
          Obj.repr ()
      | PRIM_OPAQUE ->
          Obj.repr ()
      | PRIM_PHYS_EQ ->
          Obj.repr ()
      | PRIM_PHYS_NE ->
          Obj.repr ()
      | PRIM_PROJECT_VAR ->
          Obj.repr ()
      | PRIM_SELECT_CLOSURE ->
          Obj.repr ()
      | PRIM_TAG_IMM ->
          Obj.repr ()
      | PRIM_UNTAG_IMM ->
          Obj.repr ()
      | RBRACE ->
          Obj.repr ()
      | REC ->
          Obj.repr ()
      | RPAREN ->
          Obj.repr ()
      | SEMICOLON ->
          Obj.repr ()
      | SET_OF_CLOSURES ->
          Obj.repr ()
      | SIZE ->
          Obj.repr ()
      | STAR ->
          Obj.repr ()
      | STUB ->
          Obj.repr ()
      | SWITCH ->
          Obj.repr ()
      | SYMBOL _v ->
          Obj.repr _v
      | TUPLED ->
          Obj.repr ()
      | UNIT ->
          Obj.repr ()
      | UNREACHABLE ->
          Obj.repr ()
      | VAL ->
          Obj.repr ()
      | WHERE ->
          Obj.repr ()
      | WITH ->
          Obj.repr ()
  
  and default_reduction =
    (8, "\000\006\000\153\031\168 \141\140\000l\b\000\158\000\000&\146\145\"!\000\000\000\000\133\142\000\144\003\152\000\000\135Z\151\000\000\000\000\000\000\019\000\030\000\000\024\0262\147\000\000{\000\000\000\167\000\000\023\\\000\171\000\000\137\000\149\000u\000\000\000n\029\000\000;@?><=AG\000\000\000\000\000^_\000\000\000jihV\000\000\131\000\148\150\154\157\156}\000\000\000D\000\000\129\000\000HI\000\000\028\000\000\139\000\000\000\000\000-\000\000w\000\000\005\000\007\000\000\000\000\000\021\000\017\022\000\000ba\000\000\000\000\00043\000\000\000\000\000,\004\t9P/\000\000\000\000*\000+T\169\000\000\000\000\000\000\000\000N#Q$\000\000\000J\000\000y\000\000\164\163\000\000\000\000\000\166\000\000\000\165\000\000\000Cs\000r\162\161\160\000\000\000\000\159p\000\015q\000\000\000\000X\000\r\000d\0006587\000\012\000\000\000\000\000\011LgfeM\155%\000\000\127\027:\000\000\000R\000\000\000KO\000\n\000\000\000.]\001\000\0001\002")
  
  and error =
    (73, "\b\128\000\000\003\000\128\004\002\000\000\000\000\000\000\000\000\000\000@\000\000\000\004H\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\0000\001\000\001\000\012\000\128\004\192\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000 \000\000\001\128\016\000\152\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\002\b@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\016\000\016\016@\b\000L\004\000\000\000\000D\128\000\000\000\004\000\000\000\000\000\004\000\001\000\000\000\000\017 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\192\004\000\004\000\016\002\000\019\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\136\000\000\0000\b\000@ \b@\000\000\000\b\000\004\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000@\001\000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000@\004\000$\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\016\000\016\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\002\000\000\000\000\000\000\001\000\000\000\000\000\000\128\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000 \000\000\000\002$\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\128\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\130\001\000\b\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000 0\000\128\000\000\002\003\018\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000 \000\000\002\002\016\000\000\000\000\000\001\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000(\000\000\000\000\b\005\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\000\000\b\000\000\000\000\002\004\000\000\000\"\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000 \000\b\000\000\000\000\136\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\004\000 \000\000\000\000\000\000\002\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\128\000\000\000\000\000\128\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000P\000\000\001\001\137@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\005\000\000\000\016\024\148\000\000\000\000\000\000\000\000\000\000\000\004\002 \000\001\000\128 \000\b\128\000\000\002\003\018\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\128\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\b\000P\000\000\001\001\137@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\004@\000\000\001\128@\002\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \132\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\016\b\000\129\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\002\000\000\000\000\002\000\000\000\000\000\000\004\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000 @ \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\004\000\000\000\b\004\000\000\000\002\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\002\128\000\000\b\012J\000\000\000\000\000\000\001\000\000\000\000\000\160\000\000\002\003\018\128\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\t\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000 \132\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\b!\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\006\000 \000\000\000\128\016\000\139\002\024\000\000\000HH\000E\129\b\000\000\000$$\000\"\000\000\000\000\000\002\016\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\b\000\b\000\000\000\000\000\000\000\000\004\000\000\b\128\000\000\003\000\128\004\002\000\132\000\000\000\000\128\000@\016\000\000\000\000\002\000\000\000\000\000\000\000\000\001\000\000\000\000\136\000\000\0000\b\000@ \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\004\000\000\000\000\000\000\000\000\002\000\000\000\001\016\000\000\000`\016\000\128@\000\000\000\000\000\000\000\000\002\000\000\000\000\000@\000\000\004\000\000\000\000\000\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\128?\248\000\b\144\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\002\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000@\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\n\000\000\000\000 \000\000\000\128\196\160\000\000\000\000\128\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000P\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \020\000\000\000\000\000\000\000\000\b\000\000\000\000\004\000\000\000\128\128\000\000\000\000\000\000\000\002\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\016\n\000\000\000\000\000\000\000\000\004\000\000\000\000\000\000\000\000@\000\000\000\000\016 \000\000\001\018\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\017 \000\000\000\000\000\000\000\000\000\000\000\016\000\000\003\003\002\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\002\000\000\000\000\"@\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\000\000\000\000\000@\000\000\000\004H\000\000\000\000\000\000\000\000\000\000@\000\016\000\000\000\001\018\000\000\000\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\128\b\000\000\000 \004\000&\000\128\000\000\000\002\016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\b\000\000\000\000\001\000\000\000\000\000\000\000\000\000\128\000\000\000D\000\000\000\024\004\000 \016\000\000\000\000\000\000\000\000\000\128\000\000\000\000\016\000\000\000\000\000\000\000\000\b\000\000\000\004@\000\000\001\128@\002\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000 \017\000\000\000\006\001\000\b\004\000\000\000\000\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\136\000\000\0000\b\000@ \000\000\000\000\000\000\b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000")
  
  and start =
    2
  
  and action =
    ((16, "\000\198\000\000\000\015\000\000\000\000\000\000\000\000\000\000\000\000\0002\000\000\000\000\000\168\000\000\000A\000~\000\000\000\000\000\000\000\000\000\000\000P\000\015\000\025\000\015\000\000\000\000\000\020\000\000\000\000\000\000\000\198\000\158\000\000\000\000\000\000\000\198\000\015\000\015\000\184\000\015\000\196\000\000\000\015\000\000\000\014\000\015\000\000\000\000\000\000\000\000\001\n\000\015\000\000\000t\000\222\001H\000\000\001\006\000\015\000\000\000\000\001p\000\000\000\024\001:\000\000\001.\000\000\000\029\000\000\000\015\000\252\000\015\000\000\000\000\000\021\000\168\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\238\000\015\001H\000\015\001<\000\000\000\000\000\158\001l\001\014\000\000\000\000\000\000\000\000\001&\001\014\000\000\001\160\000\000\000\000\000\000\000\000\000\000\000\000\000\031\000\015\001\162\000\000\001N\000\015\000\000\001D\000\168\000\000\000\000\001\128\000\168\000\000\001\004\000\168\000\000\000\015\001\130\000~\001\184\000~\000\000\000\156\000\168\000\000\001b\000\198\000\000\000~\000\000\0000\001\132\000\015\001(\001\182\000\000\000\003\000\000\000\000\001B\000\015\000\000\000\000\001V\000\168\001\144\000\168\001\188\000\000\000\000\001@\001\148\000~\001\200\000~\000\000\000\000\000\000\000\000\000\000\000\000\000\244\000(\000^\000<\000\000\000\000\000\000\000\000\000\000\000~\001<\001r\001*\000\015\001l\001\136\001*\000\000\000\000\000\000\000\000\001l\001\138\001*\000\000\001*\000\015\000\000\001x\000\015\000\000\000\000\001\154\000\015\001\162\000\015\001\206\000\000\000\015\001z\001:\000\000\001^\000\168\001\206\000\000\000\000\001^\000\000\000\000\000\000\000\000\001<\000\158\001P\001\154\000\000\000\000\001d\000\000\000\000\001<\000\158\001\166\000\015\000\000\001\216\000\000\000\015\000\000\000J\000\000\000\000\000\000\000\000\000\015\000\000\001\170\000\015\001|\000\015\001\222\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\208\000^\000\000\000\000\000\000\001l\001\164\000\198\000\000\001l\001\166\000\198\000\000\000\000\001\228\000\000\001z\000\198\001\150\000\000\000\000\000\000\000\198\001\152\000\000\000\000"), (16, "\000\014\001\026\002\r\000>\001\021\000\154\000=\001Y\000a\002\202\003*\003.\0032\003J\003Z\003n\003v\003z\003~\003\130\003\166\001\205\001\205\001\190\000r\001\006\001\169\001\169\002\029\001\205\002R\000M\001\017\001\205\001\205\001\026\000\018\000=\001\137\001\169\000\022\0029\0029\000\026\001\021\001\214\000=\000\161\001I\001I\000b\000a\001\021\0016\0029\000*\002\190\001\022\000\166\000\222\001j\001I\000M\000a\001\205\003\206\003\210\001\205\001\169\001\169\0029\000M\001\205\003\214\003\218\001\205\000\186\000\157\000Z\000a\001\169\001\137\002\194\002:\0029\001a\001a\001:\001\205\001\222\002N\001I\001\205\001\169\000\153\0029\001\169\001\169\001a\002\198\002\021\002\021\001I\000\006\000\170\001\245\001\245\000\n\0029\001\137\000B\0029\0029\002\021\002\165\001I\000F\001>\001\245\001I\001\237\000J\0006\000\189\002\182\001B\001F\0006\001a\000\130\001J\002%\001\213\001N\001\229\001R\000\189\000\146\000\150\001a\001\246\002\026\000\014\002\021\002%\000E\000\162\002*\001\245\001Q\001\221\000\226\001a\000\006\002\021\001a\001a\000\n\001&\001\245\002.\002\005\000a\001\237\001\177\002%\000\189\002\021\000\014\0026\002\021\002\021\001\245\001\177\002%\004\030\001\245\000\189\001\130\003\142\001i\001\253\000\022\000\222\002%\000\238\001\177\000\146\002\230\001\229\000\189\001^\001n\001\221\000\189\0029\001\190\002*\000Z\002^\000\254\001}\002%\001r\000\230\000\210\001\185\001\018\000\022\001\150\002.\000\230\001\017\001\005\001\185\003^\0005\000\186\001f\0026\001~\003\026\001\162\001\198\003\158\001\218\002\165\001\234\002\006\001\206\002\014\002&\002>\002J\002n\002v\002~\002\142\002\150\002\226\002\242\003\014\003&\0036\003>\003F\003R\003f\003\146\003\178\003\190\003\234\003\242\003\250\0046\004F\004V\004^\004g\004{"))
  
  and lhs =
    (8, "\001\000WVUUUUUUTTSRRQQPPOOONMLLKKJIHHGGFFEDCCCCCBA@??>=<<;;;;::9999999887766554321100/.--,,++**))('''&&%%%%%$$$##\"\"!!   \031\031\030\030\029\029\028\028\027\027\026\026\025\025\024\024\023\023\022\022\021\021\020\020\019\019\019\018\018\017\017\016\015\014\r\012\011\n\t\t\t\t\b\007\006\006\006\006\006\006\006\005\004\003\002\002")
  
  and goto =
    ((16, "\0004\000\000\001\014\000\000\000\000\000\000\000\000\000\000\000\000\0000\000\000\000\000\001\020\000\000\000\000\000\180\000\000\000\000\000\000\000\000\000\000\000!\000\242\000\000\001\n\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\015\000\000\000\000\000\000\000d\000\t\000\012\000\000\0001\000U\000\000\001@\000\000\000E\000\006\000\000\000\000\000\000\000\000\000\000\001|\000\000\000R\000\000\001:\000\000\000\000\0012\000\000\000\000\000\000\000\000\000\000\000\005\000\000\000\000\000\000\000@\000\000\001d\000r\001h\000\000\000\000\000\000\000J\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000F\000\000\001\182\000~\000\000\000\000\000\222\000\000\000>\000\000\000\000\000\000\000\000\000\000\0016\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\130\000\014\000\000\000\000\000\000\000z\000\000\000\000\000\014\000\000\000\000\000\000\001$\000\000\000\000\000`\000\000\000\238\000\000\000t\000|\000\132\000\000\001\176\001|\000\000\000\000\000\140\000\000\000\200\000\000\000u\000\000\001j\001Z\000\000\000\000\001R\000\000\000\000\001F\001\178\000\000\000\000\000\000\001\160\000\000\001\164\000\000\000\000\000\000\001\212\000\000\000\248\001|\001b\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001\196\001 \000\000\000\000\000\000\000\000\000\000\000\000\001|\001\152\000\000\000\254\000\b\002\004\000\000\001\012\000\000\000\000\000\000\000\000\002\b\000\000\001\022\000\000\000\000\001t\000\000\000\000\000(\000\000\000\000\000\000\001\212\000\000\001\220\000\000\000\000\001\224\000\000\002\006\000\000\001\164\001\166\000\000\000\000\000\000\001\170\000\000\000\000\000\000\000\000\001\208\002\016\001\224\002\022\000\000\000\000\001\130\000\000\000\000\001\218\002\026\000\000\000\156\000\000\000\000\000\000\001\140\000\000\001\182\000\000\000\000\000\000\000\000\001\164\000\000\000\000\001\168\000\000\001\188\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\001T\000\000\000\000\000\000\002*\000\000\000\168\000\000\002.\000\000\000\180\000\000\000\000\000\000\000\000\000\000\000\132\000\000\000\000\000\000\000\000\000J\000\000\000\000\000\000"), (16, "\000M\000\015\000\030\000;\000 \000W\000Y\0000\000M\000k\000l\000M\000m\000W\000Y\000\"\000(\000k\000l\000C\000m\000\151\0004\001\012\000\b\001\016\000\241\000y\000\169\000\170\000\t\000\187\000q\000\194\0007\000b\000\012\0003\000,\000\243\000\170\000c\0002\000D\000W\000Y\000\198\001\020\000k\000l\000\201\000m\000\250\000\170\000\198\000d\001\004\001\000\000\201\000\r\000z\000s\000n\000H\000}\000M\001\023\000e\000\127\000\170\000o\001\005\000\171\000\170\000\172\000h\000A\001\030\000o\001\005\0001\001\001\000\b\000\171\000\173\000\172\000u\000\170\000\t\001\027\001\028\000L\000\027\000\170\000V\000\173\000\171\000\024\000\172\001 \001\027\000\238\000^\001\002\001\003\000}\001\025\000\020\000\173\000o\001\005\001\011\000\171\001\021\000\172\000s\000\171\000\020\000\172\000_\000\128\000\239\001\011\000\129\000\173\000\b\000\131\000\134\000\173\001\027\000\171\000\t\000\172\001\n\000\133\001\011\000\171\000\b\000\172\000\b\000\024\000\020\000\173\000\t\000\028\000\t\000\015\001\015\000\173\000 \000\027\001\011\000\024\001\019\000\n\001\011\000\026\000\b\000\021\000b\000#\000\022\000;\000\t\000-\000c\000y\000\153\000\021\001\011\000\180\000\022\000=\000\154\000\190\001\011\000\191\000\031\000>\000g\000$\000\190\000-\000\191\000-\000-\000\190\000\141\000\191\000\181\000M\000e\000\021\000\027\000\020\000\165\001\t\001\006\000?\000|\000(\000\161\000\b\000}\000\192\000\027\0004\000\027\000\t\000\197\000y\000\182\000\193\001\007\000\b\000\200\000\b\000\242\0006\000\193\000\t\000\153\000\t\000\162\000\193\000\027\000\b\000\154\000y\000\249\000y\000\252\000\t\000.\000j\000A\000\198\000\182\0000\001\007\000\201\000\254\0000\000\136\0000\000\021\000\137\000}\000\167\000\146\000\150\000I\000\163\000K\000\145\000\155\000\168\000\175\000\183\000\184\000\188\000\157\000\195\000\159\000\214\000}\000\219\000}\000\217\000\221\000\027\000\226\000\227\000\231\000\230\000\233\000\235\000\236\000\248\001\r\0003\001\017\000\027\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\027\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\207\000\000\000\000\000\000\000\209\000\000\000\212"))
  
  and semantic_action =
    [|
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = args;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_args_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_args_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = cont;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_cont_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_cont_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let args : 'tv_simple_args = Obj.magic args in
        let cont : 'tv_continuation = Obj.magic cont in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_cont_ in
        let _endpos = _endpos_args_ in
        let _v : 'tv_apply_cont_expr = 
# 426 "flambda_parser.mly"
    ( { cont; args; trap_action = None } )
# 496 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = r;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_r_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_r_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = args;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_args_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_args_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = func;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_func_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_func_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = call_kind;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_call_kind_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_call_kind_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let e : 'tv_exn_continuation = Obj.magic e in
        let r : 'tv_continuation = Obj.magic r in
        let _4 : unit = Obj.magic _4 in
        let args : 'tv_simple_args = Obj.magic args in
        let func : 'tv_func_name_with_optional_arities = Obj.magic func in
        let call_kind : 'tv_call_kind = Obj.magic call_kind in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_call_kind_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_apply_expr = 
# 406 "flambda_parser.mly"
     ( let (func, arities) = func in {
          func;
          continuation = r;
          exn_continuation = e;
          args = args;
          call_kind;
          arities;
     } )
# 563 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_expr = 
# 344 "flambda_parser.mly"
        ( Invalid Halt_and_catch_fire )
# 588 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_atomic_expr = 
# 345 "flambda_parser.mly"
                ( Invalid Treat_as_unreachable )
# 613 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = ac;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ac_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ac_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let ac : 'tv_apply_cont_expr = Obj.magic ac in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_ac_ in
        let _v : 'tv_atomic_expr = 
# 346 "flambda_parser.mly"
                               ( Apply_cont ac )
# 645 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cases;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cases_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cases_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = scrutinee;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_scrutinee_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_scrutinee_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let cases : 'tv_switch = Obj.magic cases in
        let scrutinee : 'tv_simple = Obj.magic scrutinee in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cases_ in
        let _v : 'tv_atomic_expr = 
# 347 "flambda_parser.mly"
                                               ( Switch {scrutinee; cases} )
# 684 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let e : 'tv_apply_expr = Obj.magic e in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_atomic_expr = 
# 348 "flambda_parser.mly"
                         ( Apply e )
# 716 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = e;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let e : 'tv_expr = Obj.magic e in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_atomic_expr = 
# 349 "flambda_parser.mly"
                             ( e )
# 755 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _6;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__6_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__6_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = arg2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = arg1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg1_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _2;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = op;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_op_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_op_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let arg2 : 'tv_simple = Obj.magic arg2 in
        let _4 : unit = Obj.magic _4 in
        let arg1 : 'tv_simple = Obj.magic arg1 in
        let _2 : unit = Obj.magic _2 in
        let op : 'tv_prefix_binop = Obj.magic op in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_op_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_binop_app = 
# 254 "flambda_parser.mly"
    ( Binary (op, arg1, arg2) )
# 815 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = arg2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = op;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_op_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_op_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = arg1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_arg1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_arg1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let arg2 : 'tv_simple = Obj.magic arg2 in
        let op : 'tv_infix_binop = Obj.magic op in
        let arg1 : 'tv_simple = Obj.magic arg1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_arg1_ in
        let _endpos = _endpos_arg2_ in
        let _v : 'tv_binop_app = 
# 256 "flambda_parser.mly"
    ( Binary (Infix op, arg1, arg2) )
# 854 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _6;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__6_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__6_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = xs;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = t;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_t_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_t_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = m;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_m_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_m_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = _1;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let xs : 'tv_loption_separated_nonempty_list_COMMA_simple__ = Obj.magic xs in
        let _4 : unit = Obj.magic _4 in
        let t : 'tv_tag = Obj.magic t in
        let m : (
# 121 "flambda_parser.mly"
      (Fexpr.mutability)
# 909 "flambda_parser-in.ml"
        ) = Obj.magic m in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_block = let elts = 
# 232 "<standard.mly>"
    ( xs )
# 918 "flambda_parser-in.ml"
         in
        
# 263 "flambda_parser.mly"
    ( Variadic (Make_block (t, m), elts) )
# 923 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_block_access_field_kind = 
# 249 "flambda_parser.mly"
    ( Any_value )
# 941 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_block_access_field_kind = 
# 250 "flambda_parser.mly"
        ( Immediate )
# 966 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_boption_NOALLOC_ = 
# 133 "<standard.mly>"
    ( false )
# 984 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_boption_NOALLOC_ = 
# 135 "<standard.mly>"
    ( true )
# 1009 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_boption_TUPLED_ = 
# 133 "<standard.mly>"
    ( false )
# 1027 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_boption_TUPLED_ = 
# 135 "<standard.mly>"
    ( true )
# 1052 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_call_kind = 
# 417 "flambda_parser.mly"
    ( Function Indirect )
# 1070 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _5;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = closure_id;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_id_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_id_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = code_id;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_id_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_id_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = _2;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let _5 : unit = Obj.magic _5 in
        let closure_id : 'tv_closure_id_opt = Obj.magic closure_id in
        let code_id : 'tv_code_id = Obj.magic code_id in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__5_ in
        let _v : 'tv_call_kind = 
# 419 "flambda_parser.mly"
    ( Function (Direct { code_id; closure_id }) )
# 1123 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = noalloc;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_noalloc_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_noalloc_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let noalloc : 'tv_boption_NOALLOC_ = Obj.magic noalloc in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_noalloc_ in
        let _v : 'tv_call_kind = 
# 421 "flambda_parser.mly"
    ( C_call { alloc = not noalloc } )
# 1155 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = value;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_value_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_value_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = var;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let value : 'tv_simple = Obj.magic value in
        let _2 : unit = Obj.magic _2 in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_var_ in
        let _endpos = _endpos_value_ in
        let _v : 'tv_closure_element = 
# 393 "flambda_parser.mly"
                                                     ( { var; value; } )
# 1194 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_closure_id = 
# 509 "flambda_parser.mly"
                 ( v )
# 1219 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_closure_id_opt = 
# 513 "flambda_parser.mly"
    ( None )
# 1237 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cid;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cid_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cid_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let cid : 'tv_closure_id = Obj.magic cid in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cid_ in
        let _v : 'tv_closure_id_opt = 
# 514 "flambda_parser.mly"
                         ( Some cid )
# 1269 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _8;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__8_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__8_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = ret_arity;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_arity_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_arity_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = exn_cont;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_exn_cont_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_exn_cont_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = ret_cont;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_cont_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_cont_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _;
                    CamlinternalMenhirLib.EngineTypes.semv = _4;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
                    CamlinternalMenhirLib.EngineTypes.next = {
                      CamlinternalMenhirLib.EngineTypes.state = _;
                      CamlinternalMenhirLib.EngineTypes.semv = closure_var;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_var_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_var_;
                      CamlinternalMenhirLib.EngineTypes.next = {
                        CamlinternalMenhirLib.EngineTypes.state = _;
                        CamlinternalMenhirLib.EngineTypes.semv = params;
                        CamlinternalMenhirLib.EngineTypes.startp = _startpos_params_;
                        CamlinternalMenhirLib.EngineTypes.endp = _endpos_params_;
                        CamlinternalMenhirLib.EngineTypes.next = {
                          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                          CamlinternalMenhirLib.EngineTypes.semv = header;
                          CamlinternalMenhirLib.EngineTypes.startp = _startpos_header_;
                          CamlinternalMenhirLib.EngineTypes.endp = _endpos_header_;
                          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                        };
                      };
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _8 : unit = Obj.magic _8 in
        let ret_arity : 'tv_return_arity = Obj.magic ret_arity in
        let exn_cont : 'tv_exn_continuation_id = Obj.magic exn_cont in
        let ret_cont : 'tv_continuation_id = Obj.magic ret_cont in
        let _4 : unit = Obj.magic _4 in
        let closure_var : 'tv_variable = Obj.magic closure_var in
        let params : 'tv_kinded_args = Obj.magic params in
        let header : 'tv_code_header = Obj.magic header in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_header_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_code = 
# 174 "flambda_parser.mly"
    ( let recursive, id, newer_version_of = header in
      { id; newer_version_of; param_arity = None; ret_arity; recursive;
        params_and_body = Present { params; closure_var; ret_cont; exn_cont;
                                    body } } )
# 1353 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = ret_arity;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_arity_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_arity_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _5;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = param_arity;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_arity_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_arity_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = _3;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _2;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = header;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_header_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_header_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let ret_arity : 'tv_kinds = Obj.magic ret_arity in
        let _5 : unit = Obj.magic _5 in
        let param_arity : 'tv_kinds = Obj.magic param_arity in
        let _3 : unit = Obj.magic _3 in
        let _2 : unit = Obj.magic _2 in
        let header : 'tv_code_header = Obj.magic header in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_header_ in
        let _endpos = _endpos_ret_arity_ in
        let _v : 'tv_code = 
# 184 "flambda_parser.mly"
    ( let recursive, id, newer_version_of = header in
      { id; newer_version_of; param_arity = Some param_arity;
        ret_arity = Some ret_arity; recursive; params_and_body = Deleted } )
# 1415 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = newer_version_of;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_newer_version_of_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_newer_version_of_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = id;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_id_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_id_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = recursive;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_recursive_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_recursive_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let newer_version_of : 'tv_option___anonymous_0_ = Obj.magic newer_version_of in
        let id : 'tv_code_id = Obj.magic id in
        let recursive : 'tv_recursive = Obj.magic recursive in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_newer_version_of_ in
        let _v : 'tv_code_header = 
# 194 "flambda_parser.mly"
    ( recursive, id, newer_version_of )
# 1461 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_code_id = 
# 505 "flambda_parser.mly"
                 ( v )
# 1486 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : (
# 72 "flambda_parser.mly"
       (string * char option)
# 1507 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 483 "flambda_parser.mly"
            ( make_const_int c )
# 1515 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : (
# 63 "flambda_parser.mly"
       (float)
# 1536 "flambda_parser-in.ml"
        ) = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_const = 
# 484 "flambda_parser.mly"
              ( Naked_float c )
# 1544 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : 'tv_continuation_id = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_continuation = 
# 530 "flambda_parser.mly"
                        ( Named e )
# 1569 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_special_continuation = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_continuation = 
# 531 "flambda_parser.mly"
                             ( Special s )
# 1594 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let l : 'tv_let_expr_continuation_body_ = Obj.magic l in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_l_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_continuation_body = 
# 339 "flambda_parser.mly"
                                    ( l )
# 1619 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = a;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let a : 'tv_atomic_expr = Obj.magic a in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_a_ in
        let _v : 'tv_continuation_body = 
# 340 "flambda_parser.mly"
                    ( a )
# 1644 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = handler;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_handler_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_handler_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _4;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = params;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_params_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_params_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = name;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_name_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_name_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = exn_and_stub;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_exn_and_stub_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_exn_and_stub_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let handler : 'tv_continuation_body = Obj.magic handler in
        let _4 : unit = Obj.magic _4 in
        let params : 'tv_kinded_args = Obj.magic params in
        let name : 'tv_continuation_id = Obj.magic name in
        let exn_and_stub : 'tv_exn_and_stub = Obj.magic exn_and_stub in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_exn_and_stub_ in
        let _endpos = _endpos_handler_ in
        let _v : 'tv_continuation_handler = 
# 440 "flambda_parser.mly"
    ( let is_exn_handler, stub = exn_and_stub in
      { name; params; stub; is_exn_handler; handler } )
# 1698 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 66 "flambda_parser.mly"
       (string)
# 1719 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_continuation_id = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 526 "flambda_parser.mly"
              ( make_located e (_startpos, _endpos) )
# 1729 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_exn_and_stub = 
# 430 "flambda_parser.mly"
    ( false, false )
# 1747 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_exn_and_stub = 
# 431 "flambda_parser.mly"
         ( false, true )
# 1772 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_exn_and_stub = 
# 432 "flambda_parser.mly"
        ( true, false )
# 1797 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_exn_and_stub = 
# 433 "flambda_parser.mly"
             ( true, true )
# 1829 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__2_ in
        let _v : 'tv_exn_and_stub = 
# 434 "flambda_parser.mly"
             ( true, true )
# 1861 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cont;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cont_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cont_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let cont : 'tv_continuation = Obj.magic cont in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cont_ in
        let _v : 'tv_exn_continuation = 
# 147 "flambda_parser.mly"
                             ( cont )
# 1893 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = cont;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_cont_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_cont_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let cont : 'tv_continuation_id = Obj.magic cont in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_cont_ in
        let _v : 'tv_exn_continuation_id = 
# 150 "flambda_parser.mly"
                                ( cont )
# 1925 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _4;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = after;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_after_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_after_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _2;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = before;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_before_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_before_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let after : 'tv_module_ = Obj.magic after in
        let _2 : unit = Obj.magic _2 in
        let before : 'tv_module_ = Obj.magic before in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_before_ in
        let _endpos = _endpos__4_ in
        let _v : (
# 118 "flambda_parser.mly"
      (Fexpr.expect_test_spec)
# 1971 "flambda_parser-in.ml"
        ) = 
# 134 "flambda_parser.mly"
    ( { before; after } )
# 1975 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let l : 'tv_let_expr_expr_ = Obj.magic l in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_l_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_expr = 
# 318 "flambda_parser.mly"
                       ( l )
# 2000 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = i;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let i : 'tv_inner_expr = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : 'tv_expr = 
# 319 "flambda_parser.mly"
                   ( i )
# 2025 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _2;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = body;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let _2 : unit = Obj.magic _2 in
        let body : 'tv_module_ = Obj.magic body in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_body_ in
        let _endpos = _endpos__2_ in
        let _v : (
# 117 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 2057 "flambda_parser-in.ml"
        ) = 
# 129 "flambda_parser.mly"
    ( body )
# 2061 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = closure_id;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_id_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_id_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = code_id;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_id_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_id_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = is_tupled;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_is_tupled_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_is_tupled_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let closure_id : 'tv_closure_id_opt = Obj.magic closure_id in
        let code_id : 'tv_code_id = Obj.magic code_id in
        let is_tupled : 'tv_boption_TUPLED_ = Obj.magic is_tupled in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_closure_id_ in
        let _v : 'tv_fun_decl = 
# 399 "flambda_parser.mly"
    ( { code_id; closure_id; is_tupled } )
# 2107 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = n;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_n_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_n_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let n : 'tv_name = Obj.magic n in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_n_ in
        let _endpos = _endpos_n_ in
        let _v : 'tv_func_name_with_optional_arities = 
# 493 "flambda_parser.mly"
             ( n, None )
# 2132 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _7;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__7_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__7_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = ret_arity;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_ret_arity_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_ret_arity_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _5;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__5_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__5_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = params_arity;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_params_arity_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_params_arity_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _3;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _;
                    CamlinternalMenhirLib.EngineTypes.semv = n;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos_n_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos_n_;
                    CamlinternalMenhirLib.EngineTypes.next = {
                      CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                      CamlinternalMenhirLib.EngineTypes.semv = _1;
                      CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                      CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                      CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                    };
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _7 : unit = Obj.magic _7 in
        let ret_arity : 'tv_kinds = Obj.magic ret_arity in
        let _5 : unit = Obj.magic _5 in
        let params_arity : 'tv_kinds = Obj.magic params_arity in
        let _3 : unit = Obj.magic _3 in
        let n : 'tv_name = Obj.magic n in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__7_ in
        let _v : 'tv_func_name_with_optional_arities = 
# 496 "flambda_parser.mly"
    ( n, Some ({ params_arity; ret_arity } : function_arities) )
# 2199 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 227 "flambda_parser.mly"
         ( Plus )
# 2224 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 228 "flambda_parser.mly"
            ( Plusdot )
# 2249 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 229 "flambda_parser.mly"
          ( Minus )
# 2274 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_infix_binop = 
# 230 "flambda_parser.mly"
             ( Minusdot )
# 2299 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = w;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_w_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_w_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let w : 'tv_where_expr = Obj.magic w in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_w_ in
        let _endpos = _endpos_w_ in
        let _v : 'tv_inner_expr = 
# 328 "flambda_parser.mly"
                   ( w )
# 2324 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = a;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let a : 'tv_atomic_expr = Obj.magic a in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_a_ in
        let _endpos = _endpos_a_ in
        let _v : 'tv_inner_expr = 
# 329 "flambda_parser.mly"
                    ( a )
# 2349 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2374 "flambda_parser-in.ml"
        ) = 
# 282 "flambda_parser.mly"
        ( Value )
# 2378 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2403 "flambda_parser-in.ml"
        ) = 
# 283 "flambda_parser.mly"
        ( Naked_number Naked_immediate )
# 2407 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2432 "flambda_parser-in.ml"
        ) = 
# 284 "flambda_parser.mly"
               ( Naked_number Naked_float )
# 2436 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2461 "flambda_parser-in.ml"
        ) = 
# 285 "flambda_parser.mly"
          ( Naked_number Naked_int32 )
# 2465 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2490 "flambda_parser-in.ml"
        ) = 
# 286 "flambda_parser.mly"
          ( Naked_number Naked_int64 )
# 2494 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2519 "flambda_parser-in.ml"
        ) = 
# 287 "flambda_parser.mly"
              ( Naked_number Naked_nativeint )
# 2523 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2548 "flambda_parser-in.ml"
        ) = 
# 288 "flambda_parser.mly"
               ( Fabricated )
# 2552 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_kind_arg_opt = 
# 300 "flambda_parser.mly"
    ( None )
# 2570 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = k;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let k : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2604 "flambda_parser-in.ml"
        ) = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kind_arg_opt = 
# 301 "flambda_parser.mly"
                             ( Some k )
# 2613 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = v;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let v : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = Obj.magic v in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_kinded_args = 
# 445 "flambda_parser.mly"
                                                                      ( v )
# 2652 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_kinded_args = 
# 446 "flambda_parser.mly"
    ( [] )
# 2670 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = param;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let param : 'tv_variable = Obj.magic param in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_param_ in
        let _endpos = _endpos_param_ in
        let _v : 'tv_kinded_variable = 
# 473 "flambda_parser.mly"
                     ( { param; kind = None } )
# 2695 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = kind;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_kind_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_kind_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = param;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_param_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_param_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let kind : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 2728 "flambda_parser-in.ml"
        ) = Obj.magic kind in
        let _2 : unit = Obj.magic _2 in
        let param : 'tv_variable = Obj.magic param in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_param_ in
        let _endpos = _endpos_kind_ in
        let _v : 'tv_kinded_variable = 
# 474 "flambda_parser.mly"
                                         ( { param; kind = Some kind } )
# 2738 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_kinds = 
# 291 "flambda_parser.mly"
         ( [] )
# 2763 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = ks;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ks_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ks_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let ks : 'tv_separated_nonempty_list_STAR_kind_ = Obj.magic ks in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ks_ in
        let _endpos = _endpos_ks_ in
        let _v : 'tv_kinds = 
# 292 "flambda_parser.mly"
                                             ( ks )
# 2788 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_continuation_body = Obj.magic body in
        let _3 : unit = Obj.magic _3 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic bindings in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_bindings_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let__continuation_body_ = 
# 376 "flambda_parser.mly"
    ( ({ bindings; closure_elements; body } : let_) )
# 2834 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _3 : unit = Obj.magic _3 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic bindings in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_bindings_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let__expr_ = 
# 376 "flambda_parser.mly"
    ( ({ bindings; closure_elements; body } : let_) )
# 2880 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = defining_expr;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_defining_expr_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_defining_expr_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = v;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let defining_expr : (
# 122 "flambda_parser.mly"
      (Fexpr.named)
# 2913 "flambda_parser-in.ml"
        ) = Obj.magic defining_expr in
        let _2 : unit = Obj.magic _2 in
        let v : 'tv_kinded_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_defining_expr_ in
        let _v : 'tv_let_binding = 
# 381 "flambda_parser.mly"
      ( let { param = var; kind } = v in { var; kind; defining_expr } )
# 2923 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let l : 'tv_let__continuation_body_ = Obj.magic l in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_let_expr_continuation_body_ = 
# 323 "flambda_parser.mly"
                       ( Let l )
# 2955 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = ls;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ls_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ls_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let ls : 'tv_let_symbol_continuation_body_ = Obj.magic ls in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ls_ in
        let _endpos = _endpos_ls_ in
        let _v : 'tv_let_expr_continuation_body_ = 
# 324 "flambda_parser.mly"
                          ( Let_symbol ls )
# 2980 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = l;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_l_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_l_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let l : 'tv_let__expr_ = Obj.magic l in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_l_ in
        let _v : 'tv_let_expr_expr_ = 
# 323 "flambda_parser.mly"
                       ( Let l )
# 3012 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = ls;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ls_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ls_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let ls : 'tv_let_symbol_expr_ = Obj.magic ls in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_ls_ in
        let _endpos = _endpos_ls_ in
        let _v : 'tv_let_expr_expr_ = 
# 324 "flambda_parser.mly"
                          ( Let_symbol ls )
# 3037 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _4;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_continuation_body = Obj.magic body in
        let _4 : unit = Obj.magic _4 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_symbol_binding_ = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let_symbol_continuation_body_ = 
# 156 "flambda_parser.mly"
                     ( { bindings; closure_elements; body } )
# 3090 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _4;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = closure_elements;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_closure_elements_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = bindings;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _4 : unit = Obj.magic _4 in
        let closure_elements : 'tv_with_closure_elements_opt = Obj.magic closure_elements in
        let bindings : 'tv_separated_nonempty_list_AND_symbol_binding_ = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_let_symbol_expr_ = 
# 156 "flambda_parser.mly"
                     ( { bindings; closure_elements; body } )
# 3143 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_ANDWHERE_continuation_handler__ = 
# 142 "<standard.mly>"
    ( [] )
# 3161 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_ANDWHERE_continuation_handler__ = 
# 144 "<standard.mly>"
    ( x )
# 3186 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_of_kind_value__ = 
# 142 "<standard.mly>"
    ( [] )
# 3204 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_of_kind_value__ = 
# 144 "<standard.mly>"
    ( x )
# 3229 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_simple__ = 
# 142 "<standard.mly>"
    ( [] )
# 3247 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_COMMA_simple_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_COMMA_simple__ = 
# 144 "<standard.mly>"
    ( x )
# 3272 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_PIPE_switch_case__ = 
# 142 "<standard.mly>"
    ( [] )
# 3290 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_PIPE_switch_case_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_PIPE_switch_case__ = 
# 144 "<standard.mly>"
    ( x )
# 3315 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 142 "<standard.mly>"
    ( [] )
# 3333 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = 
# 144 "<standard.mly>"
    ( x )
# 3358 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = body;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let body : 'tv_expr = Obj.magic body in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_body_ in
        let _endpos = _endpos_body_ in
        let _v : 'tv_module_ = 
# 143 "flambda_parser.mly"
    ( { body } )
# 3383 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.mutability)
# 3408 "flambda_parser-in.ml"
        ) = 
# 244 "flambda_parser.mly"
            ( Mutable )
# 3412 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.mutability)
# 3437 "flambda_parser-in.ml"
        ) = 
# 245 "flambda_parser.mly"
                     ( Immutable_unique )
# 3441 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : (
# 121 "flambda_parser.mly"
      (Fexpr.mutability)
# 3459 "flambda_parser-in.ml"
        ) = 
# 246 "flambda_parser.mly"
    ( Immutable )
# 3463 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_name = 
# 488 "flambda_parser.mly"
               ( (Symbol s:name) )
# 3488 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_name = 
# 489 "flambda_parser.mly"
                 ( (Var v:name) )
# 3513 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_simple = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (
# 122 "flambda_parser.mly"
      (Fexpr.named)
# 3538 "flambda_parser-in.ml"
        ) = 
# 267 "flambda_parser.mly"
               ( Simple s )
# 3542 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = a;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_a_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_a_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = u;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_u_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_u_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let a : 'tv_simple = Obj.magic a in
        let u : 'tv_unop = Obj.magic u in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_u_ in
        let _endpos = _endpos_a_ in
        let _v : (
# 122 "flambda_parser.mly"
      (Fexpr.named)
# 3574 "flambda_parser-in.ml"
        ) = 
# 268 "flambda_parser.mly"
                        ( Prim (Unary (u, a)) )
# 3578 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = b;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_b_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_b_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let b : 'tv_binop_app = Obj.magic b in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_b_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 122 "flambda_parser.mly"
      (Fexpr.named)
# 3603 "flambda_parser-in.ml"
        ) = 
# 269 "flambda_parser.mly"
                  ( Prim b )
# 3607 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = b;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_b_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_b_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let b : 'tv_block = Obj.magic b in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_b_ in
        let _endpos = _endpos_b_ in
        let _v : (
# 122 "flambda_parser.mly"
      (Fexpr.named)
# 3632 "flambda_parser-in.ml"
        ) = 
# 270 "flambda_parser.mly"
              ( Prim b )
# 3636 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : 'tv_fun_decl = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : (
# 122 "flambda_parser.mly"
      (Fexpr.named)
# 3661 "flambda_parser-in.ml"
        ) = 
# 271 "flambda_parser.mly"
                 ( Closure c )
# 3665 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : (
# 123 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3690 "flambda_parser-in.ml"
        ) = 
# 467 "flambda_parser.mly"
               ( Symbol s )
# 3694 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : (
# 123 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3719 "flambda_parser-in.ml"
        ) = 
# 468 "flambda_parser.mly"
                 ( Dynamically_computed v )
# 3723 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = i;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let i : (
# 72 "flambda_parser.mly"
       (string * char option)
# 3744 "flambda_parser-in.ml"
        ) = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : (
# 123 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 3752 "flambda_parser-in.ml"
        ) = let _endpos = _endpos_i_ in
        let _startpos = _startpos_i_ in
        
# 469 "flambda_parser.mly"
            ( Tagged_immediate ( make_tagged_immediate ~loc:(_startpos, _endpos) i ) )
# 3758 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option_PIPE_ = 
# 114 "<standard.mly>"
    ( None )
# 3776 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : unit = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_option_PIPE_ = 
# 116 "<standard.mly>"
    ( Some x )
# 3801 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option___anonymous_0_ = 
# 114 "<standard.mly>"
    ( None )
# 3819 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = id;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_id_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_id_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let id : 'tv_code_id = Obj.magic id in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_id_ in
        let _v : 'tv_option___anonymous_0_ = let x = 
# 193 "flambda_parser.mly"
                                                             ( id )
# 3851 "flambda_parser-in.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3856 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_option___anonymous_1_ = 
# 114 "<standard.mly>"
    ( None )
# 3874 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = size;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_size_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_size_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let size : 'tv_targetint = Obj.magic size in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_size_ in
        let _v : 'tv_option___anonymous_1_ = let x = 
# 237 "flambda_parser.mly"
                                         ( size )
# 3906 "flambda_parser-in.ml"
         in
        
# 116 "<standard.mly>"
    ( Some x )
# 3911 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = field_kind;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_field_kind_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_field_kind_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = size;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_size_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_size_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = tag;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_tag_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_tag_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = mutability;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_mutability_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_mutability_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                  CamlinternalMenhirLib.EngineTypes.semv = _1;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                  CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                };
              };
            };
          };
        } = _menhir_stack in
        let field_kind : 'tv_block_access_field_kind = Obj.magic field_kind in
        let size : 'tv_option___anonymous_1_ = Obj.magic size in
        let tag : 'tv_tag = Obj.magic tag in
        let mutability : (
# 121 "flambda_parser.mly"
      (Fexpr.mutability)
# 3959 "flambda_parser-in.ml"
        ) = Obj.magic mutability in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_field_kind_ in
        let _v : 'tv_prefix_binop = 
# 239 "flambda_parser.mly"
    ( Block_load (Values { tag; size; field_kind }, mutability) )
# 3968 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = k;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let k : 'tv_kind_arg_opt = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_k_ in
        let _v : 'tv_prefix_binop = 
# 240 "flambda_parser.mly"
                                   ( Phys_equal(k, Eq) )
# 4000 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = k;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let k : 'tv_kind_arg_opt = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_k_ in
        let _v : 'tv_prefix_binop = 
# 241 "flambda_parser.mly"
                                   ( Phys_equal(k, Neq) )
# 4032 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_recursive = 
# 210 "flambda_parser.mly"
    ( Nonrecursive )
# 4050 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_recursive = 
# 211 "flambda_parser.mly"
        ( Recursive )
# 4075 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_return_arity = 
# 295 "flambda_parser.mly"
    ( None )
# 4093 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = k;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_k_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_k_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let k : 'tv_kinds = Obj.magic k in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_k_ in
        let _v : 'tv_return_arity = 
# 296 "flambda_parser.mly"
                    ( Some k )
# 4125 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_let_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4150 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_AND_let_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_let_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_let_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4189 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_static_closure_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_static_closure_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4214 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_AND_static_closure_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_static_closure_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_static_closure_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4253 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_symbol_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_AND_symbol_binding_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4278 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_AND_symbol_binding_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_symbol_binding = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_AND_symbol_binding_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4317 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_continuation_handler = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4342 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_continuation_handler = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_ANDWHERE_continuation_handler_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4381 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_kinded_variable = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4406 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_kinded_variable = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_kinded_variable_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4445 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 123 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4466 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4474 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 123 "flambda_parser.mly"
      (Fexpr.of_kind_value)
# 4509 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_of_kind_value_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4517 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_simple = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_COMMA_simple_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4542 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_COMMA_simple_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_simple = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_COMMA_simple_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4581 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_switch_case = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_PIPE_switch_case_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4606 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_PIPE_switch_case_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_switch_case = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_PIPE_switch_case_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4645 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : 'tv_closure_element = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4670 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : 'tv_closure_element = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_SEMICOLON_closure_element_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4709 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = x;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let x : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 4730 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_x_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 241 "<standard.mly>"
    ( [ x ] )
# 4738 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = x;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_x_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_x_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let xs : 'tv_separated_nonempty_list_STAR_kind_ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let x : (
# 120 "flambda_parser.mly"
      (Fexpr.kind)
# 4773 "flambda_parser-in.ml"
        ) = Obj.magic x in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_x_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_separated_nonempty_list_STAR_kind_ = 
# 243 "<standard.mly>"
    ( x :: xs )
# 4781 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_simple = 
# 499 "flambda_parser.mly"
               ( Symbol s )
# 4806 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_v_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_v_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let v : 'tv_variable = Obj.magic v in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_v_ in
        let _endpos = _endpos_v_ in
        let _v : 'tv_simple = 
# 500 "flambda_parser.mly"
                 ( Var v )
# 4831 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = c;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_c_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_c_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let c : 'tv_const = Obj.magic c in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_c_ in
        let _endpos = _endpos_c_ in
        let _v : 'tv_simple = 
# 501 "flambda_parser.mly"
              ( Const c )
# 4856 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_simple_args = 
# 478 "flambda_parser.mly"
    ( [] )
# 4874 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _3;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = s;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = _1;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let _3 : unit = Obj.magic _3 in
        let s : 'tv_separated_nonempty_list_COMMA_simple_ = Obj.magic s in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__3_ in
        let _v : 'tv_simple_args = 
# 479 "flambda_parser.mly"
                                                             ( s )
# 4913 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_special_continuation = 
# 535 "flambda_parser.mly"
         ( Done : special_continuation )
# 4938 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_special_continuation = 
# 536 "flambda_parser.mly"
          ( Error : special_continuation )
# 4963 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = fun_decl;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_fun_decl_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_fun_decl_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = symbol;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_symbol_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_symbol_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let fun_decl : 'tv_fun_decl = Obj.magic fun_decl in
        let _2 : unit = Obj.magic _2 in
        let symbol : 'tv_symbol = Obj.magic symbol in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_symbol_ in
        let _endpos = _endpos_fun_decl_ in
        let _v : 'tv_static_closure_binding = 
# 199 "flambda_parser.mly"
    ( { symbol; fun_decl } )
# 5002 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _6;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__6_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__6_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = xs;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = tag;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_tag_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_tag_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = m;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos_m_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos_m_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = _1;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let xs : 'tv_loption_separated_nonempty_list_COMMA_of_kind_value__ = Obj.magic xs in
        let _4 : unit = Obj.magic _4 in
        let tag : 'tv_tag = Obj.magic tag in
        let m : (
# 121 "flambda_parser.mly"
      (Fexpr.mutability)
# 5057 "flambda_parser-in.ml"
        ) = Obj.magic m in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_static_part = let elements = 
# 232 "<standard.mly>"
    ( xs )
# 5066 "flambda_parser-in.ml"
         in
        
# 456 "flambda_parser.mly"
    ( (Block { tag; mutability = m; elements } : static_part) )
# 5071 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _4;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = elements;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_elements_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_elements_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = bindings;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_bindings_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_bindings_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let elements : 'tv_with_closure_elements_opt = Obj.magic elements in
        let bindings : 'tv_separated_nonempty_list_AND_static_closure_binding_ = Obj.magic bindings in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_static_set_of_closures = 
# 207 "flambda_parser.mly"
    ( { bindings; elements } )
# 5117 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = sp;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_sp_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_sp_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = s;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let sp : 'tv_static_part = Obj.magic sp in
        let _2 : unit = Obj.magic _2 in
        let s : 'tv_symbol = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_sp_ in
        let _v : (
# 119 "flambda_parser.mly"
      (Fexpr.static_structure)
# 5156 "flambda_parser-in.ml"
        ) = 
# 450 "flambda_parser.mly"
    ( { symbol = s; kind = None; defining_expr = sp } )
# 5160 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
            CamlinternalMenhirLib.EngineTypes.semv = _1;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
            CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
          };
        } = _menhir_stack in
        let xs : 'tv_loption_separated_nonempty_list_PIPE_switch_case__ = Obj.magic xs in
        let _1 : 'tv_option_PIPE_ = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_switch = let cs = 
# 232 "<standard.mly>"
    ( xs )
# 5192 "flambda_parser-in.ml"
         in
        
# 279 "flambda_parser.mly"
                                                         ( cs )
# 5197 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = ac;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_ac_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_ac_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _2;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
              CamlinternalMenhirLib.EngineTypes.semv = i;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
              CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
            };
          };
        } = _menhir_stack in
        let ac : 'tv_apply_cont_expr = Obj.magic ac in
        let _2 : unit = Obj.magic _2 in
        let i : 'tv_tag = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_ac_ in
        let _v : 'tv_switch_case = 
# 275 "flambda_parser.mly"
                                                ( i,ac )
# 5236 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 95 "flambda_parser.mly"
      (string)
# 5257 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_symbol = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 518 "flambda_parser.mly"
               ( make_located e (_startpos, _endpos) )
# 5267 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : (
# 119 "flambda_parser.mly"
      (Fexpr.static_structure)
# 5288 "flambda_parser-in.ml"
        ) = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 160 "flambda_parser.mly"
                         ( Block_like s )
# 5296 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = code;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_code_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_code_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let code : 'tv_code = Obj.magic code in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_code_ in
        let _endpos = _endpos_code_ in
        let _v : 'tv_symbol_binding = 
# 161 "flambda_parser.mly"
                ( Code code )
# 5321 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_static_closure_binding = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 162 "flambda_parser.mly"
                               ( Closure s )
# 5346 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = s;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_s_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_s_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let s : 'tv_static_set_of_closures = Obj.magic s in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_s_ in
        let _endpos = _endpos_s_ in
        let _v : 'tv_symbol_binding = 
# 163 "flambda_parser.mly"
                               ( Set_of_closures s )
# 5371 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = tag;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_tag_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_tag_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let tag : (
# 72 "flambda_parser.mly"
       (string * char option)
# 5392 "flambda_parser-in.ml"
        ) = Obj.magic tag in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_tag_ in
        let _endpos = _endpos_tag_ in
        let _v : 'tv_tag = let _endpos = _endpos_tag_ in
        let _startpos = _startpos_tag_ in
        
# 463 "flambda_parser.mly"
            ( make_tag ~loc:(make_loc (_startpos, _endpos)) tag )
# 5402 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = i;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_i_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_i_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let i : (
# 72 "flambda_parser.mly"
       (string * char option)
# 5423 "flambda_parser-in.ml"
        ) = Obj.magic i in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_i_ in
        let _endpos = _endpos_i_ in
        let _v : 'tv_targetint = 
# 460 "flambda_parser.mly"
          ( make_targetint i )
# 5431 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 215 "flambda_parser.mly"
                 ( Get_tag )
# 5456 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 216 "flambda_parser.mly"
                ( Is_int )
# 5481 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 217 "flambda_parser.mly"
                ( Opaque_identity )
# 5506 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 218 "flambda_parser.mly"
                 ( Tag_imm )
# 5531 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = _1;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__1_ in
        let _v : 'tv_unop = 
# 219 "flambda_parser.mly"
                   ( Untag_imm )
# 5556 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = var;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_var_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_var_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = _3;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos__3_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos__3_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = project_from;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos_project_from_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos_project_from_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let var : 'tv_var_within_closure = Obj.magic var in
        let _3 : unit = Obj.magic _3 in
        let project_from : 'tv_closure_id = Obj.magic project_from in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos_var_ in
        let _v : 'tv_unop = 
# 221 "flambda_parser.mly"
    ( Project_var { project_from; var } )
# 5602 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _6;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__6_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__6_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = move_to;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_move_to_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_move_to_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _4;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _;
                CamlinternalMenhirLib.EngineTypes.semv = move_from;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_move_from_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_move_from_;
                CamlinternalMenhirLib.EngineTypes.next = {
                  CamlinternalMenhirLib.EngineTypes.state = _;
                  CamlinternalMenhirLib.EngineTypes.semv = _2;
                  CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
                  CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
                  CamlinternalMenhirLib.EngineTypes.next = {
                    CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                    CamlinternalMenhirLib.EngineTypes.semv = _1;
                    CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                    CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                    CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
                  };
                };
              };
            };
          };
        } = _menhir_stack in
        let _6 : unit = Obj.magic _6 in
        let move_to : 'tv_closure_id = Obj.magic move_to in
        let _4 : unit = Obj.magic _4 in
        let move_from : 'tv_closure_id = Obj.magic move_from in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__6_ in
        let _v : 'tv_unop = 
# 224 "flambda_parser.mly"
    ( Select_closure { move_from; move_to } )
# 5662 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 66 "flambda_parser.mly"
       (string)
# 5683 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_var_within_closure = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 540 "flambda_parser.mly"
              ( make_located e (_startpos, _endpos) )
# 5693 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = e;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_e_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_e_;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        } = _menhir_stack in
        let e : (
# 66 "flambda_parser.mly"
       (string)
# 5714 "flambda_parser-in.ml"
        ) = Obj.magic e in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_e_ in
        let _endpos = _endpos_e_ in
        let _v : 'tv_variable = let _endpos = _endpos_e_ in
        let _startpos = _startpos_e_ in
        
# 522 "flambda_parser.mly"
              ( make_located e (_startpos, _endpos) )
# 5724 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = xs;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = recursive;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_recursive_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_recursive_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _2;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = body;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos_body_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos_body_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let xs : 'tv_loption_separated_nonempty_list_ANDWHERE_continuation_handler__ = Obj.magic xs in
        let recursive : 'tv_recursive = Obj.magic recursive in
        let _2 : unit = Obj.magic _2 in
        let body : 'tv_inner_expr = Obj.magic body in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos_body_ in
        let _endpos = _endpos_xs_ in
        let _v : 'tv_where_expr = let handlers = 
# 232 "<standard.mly>"
    ( xs )
# 5770 "flambda_parser-in.ml"
         in
        
# 335 "flambda_parser.mly"
     ( Let_cont { recursive; body; handlers } )
# 5775 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let _menhir_s = _menhir_env.CamlinternalMenhirLib.EngineTypes.current in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _endpos = _startpos in
        let _v : 'tv_with_closure_elements_opt = 
# 385 "flambda_parser.mly"
    ( None )
# 5793 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
      (fun _menhir_env ->
        let _menhir_stack = _menhir_env.CamlinternalMenhirLib.EngineTypes.stack in
        let {
          CamlinternalMenhirLib.EngineTypes.state = _;
          CamlinternalMenhirLib.EngineTypes.semv = _4;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos__4_;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos__4_;
          CamlinternalMenhirLib.EngineTypes.next = {
            CamlinternalMenhirLib.EngineTypes.state = _;
            CamlinternalMenhirLib.EngineTypes.semv = xs;
            CamlinternalMenhirLib.EngineTypes.startp = _startpos_xs_;
            CamlinternalMenhirLib.EngineTypes.endp = _endpos_xs_;
            CamlinternalMenhirLib.EngineTypes.next = {
              CamlinternalMenhirLib.EngineTypes.state = _;
              CamlinternalMenhirLib.EngineTypes.semv = _2;
              CamlinternalMenhirLib.EngineTypes.startp = _startpos__2_;
              CamlinternalMenhirLib.EngineTypes.endp = _endpos__2_;
              CamlinternalMenhirLib.EngineTypes.next = {
                CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
                CamlinternalMenhirLib.EngineTypes.semv = _1;
                CamlinternalMenhirLib.EngineTypes.startp = _startpos__1_;
                CamlinternalMenhirLib.EngineTypes.endp = _endpos__1_;
                CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
              };
            };
          };
        } = _menhir_stack in
        let _4 : unit = Obj.magic _4 in
        let xs : 'tv_loption_separated_nonempty_list_SEMICOLON_closure_element__ = Obj.magic xs in
        let _2 : unit = Obj.magic _2 in
        let _1 : unit = Obj.magic _1 in
        let _endpos__0_ = _menhir_stack.CamlinternalMenhirLib.EngineTypes.endp in
        let _startpos = _startpos__1_ in
        let _endpos = _endpos__4_ in
        let _v : 'tv_with_closure_elements_opt = let elements = 
# 232 "<standard.mly>"
    ( xs )
# 5839 "flambda_parser-in.ml"
         in
        
# 389 "flambda_parser.mly"
    ( Some elements )
# 5844 "flambda_parser-in.ml"
         in
        {
          CamlinternalMenhirLib.EngineTypes.state = _menhir_s;
          CamlinternalMenhirLib.EngineTypes.semv = Obj.repr _v;
          CamlinternalMenhirLib.EngineTypes.startp = _startpos;
          CamlinternalMenhirLib.EngineTypes.endp = _endpos;
          CamlinternalMenhirLib.EngineTypes.next = _menhir_stack;
        });
    |]
  
  and trace =
    None
  
end

module MenhirInterpreter = struct
  
  module ET = CamlinternalMenhirLib.TableInterpreter.MakeEngineTable (Tables)
  
  module TI = CamlinternalMenhirLib.Engine.Make (ET)
  
  include TI
  
end

let flambda_unit =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 284 lexer lexbuf) : (
# 117 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5875 "flambda_parser-in.ml"
    ))

and expect_test_spec =
  fun lexer lexbuf ->
    (Obj.magic (MenhirInterpreter.entry 0 lexer lexbuf) : (
# 118 "flambda_parser.mly"
      (Fexpr.expect_test_spec)
# 5883 "flambda_parser-in.ml"
    ))

module Incremental = struct
  
  let flambda_unit =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 284 initial_position) : (
# 117 "flambda_parser.mly"
      (Fexpr.flambda_unit)
# 5893 "flambda_parser-in.ml"
      ) MenhirInterpreter.checkpoint)
  
  and expect_test_spec =
    fun initial_position ->
      (Obj.magic (MenhirInterpreter.start 0 initial_position) : (
# 118 "flambda_parser.mly"
      (Fexpr.expect_test_spec)
# 5901 "flambda_parser-in.ml"
      ) MenhirInterpreter.checkpoint)
  
end

# 542 "flambda_parser.mly"
  

# 5909 "flambda_parser-in.ml"

# 269 "<standard.mly>"
  

# 5914 "flambda_parser-in.ml"
