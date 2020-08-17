
(* The type of tokens. *)

type token = 
  | WITH
  | WHERE
  | VAL
  | UNREACHABLE
  | UNIT
  | TUPLED
  | TAG_IMM
  | SYMBOL of (string)
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
  | IS_INT
  | INT64
  | INT32
  | INT of (string * char option)
  | IN
  | IMMUTABLE_UNIQUE
  | IMM
  | IDENT of (string)
  | HCF
  | GET_TAG
  | FLOAT_KIND
  | FLOAT of (float)
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
  | BLOCK_LOAD
  | BLOCK
  | BIGARROW
  | AT
  | APPLY
  | ANDWHERE
  | AND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val flambda_unit: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.flambda_unit)

val expect_test_spec: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Fexpr.expect_test_spec)

module MenhirInterpreter : sig
  
  (* The incremental API. *)
  
  include CamlinternalMenhirLib.IncrementalEngine.INCREMENTAL_ENGINE
    with type token = token
  
end

(* The entry point(s) to the incremental API. *)

module Incremental : sig
  
  val flambda_unit: Lexing.position -> (Fexpr.flambda_unit) MenhirInterpreter.checkpoint
  
  val expect_test_spec: Lexing.position -> (Fexpr.expect_test_spec) MenhirInterpreter.checkpoint
  
end
