(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 1996 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* Module [Int32]: 32-bit integers *)

external neg : int32 -> int32 = "%int32_neg"
external add : int32 -> int32 -> int32 = "%int32_add"
external sub : int32 -> int32 -> int32 = "%int32_sub"
external mul : int32 -> int32 -> int32 = "%int32_mul"
external div : int32 -> int32 -> int32 = "%int32_div"
external rem : int32 -> int32 -> int32 = "%int32_mod"
external logand : int32 -> int32 -> int32 = "%int32_and"
external logor : int32 -> int32 -> int32 = "%int32_or"
external logxor : int32 -> int32 -> int32 = "%int32_xor"
external shift_left : int32 -> int -> int32 = "%int32_lsl"
external shift_right : int32 -> int -> int32 = "%int32_asr"
external shift_right_logical : int32 -> int -> int32 = "%int32_lsr"
external of_int : int -> int32 = "%int32_of_int"
external to_int : int32 -> int = "%int32_to_int"
external of_float : float -> int32
  = "caml_int32_of_float" "caml_int32_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external to_float : int32 -> float
  = "caml_int32_to_float" "caml_int32_to_float_unboxed"
  [@@unboxed] [@@noalloc]
external bits_of_float : float -> int32
  = "caml_int32_bits_of_float" "caml_int32_bits_of_float_unboxed"
  [@@unboxed] [@@noalloc]
external float_of_bits : int32 -> float
  = "caml_int32_float_of_bits" "caml_int32_float_of_bits_unboxed"
  [@@unboxed] [@@noalloc]

let zero = 0l
let one = 1l
let minus_one = -1l
let succ n = add n 1l
let pred n = sub n 1l
let abs n = if n >= 0l then n else neg n
let min_int = 0x80000000l
let max_int = 0x7FFFFFFFl
let lognot n = logxor n (-1l)

let to_int_unsigned =
  match Sys.word_size with
  | 32 ->
      let max_int = of_int Pervasives.max_int in
      fun n -> to_int (logand n max_int)
  | 64 ->
      let move = int_of_string "0x1_0000_0000" in (* So that it compiles in 32-bit *)
      fun n -> let i = to_int n in if i < 0 then i + move else i
  | _ ->
      assert false

external format : string -> int32 -> string = "caml_int32_format"
let to_string n = format "%d" n

external of_string : string -> int32 = "caml_int32_of_string"

let of_string_opt s =
  (* TODO: expose a non-raising primitive directly. *)
  try Some (of_string s)
  with Failure _ -> None

type t = int32

let compare (x: t) (y: t) = Pervasives.compare x y
let equal (x: t) (y: t) = compare x y = 0

let compare_unsigned n m =
  compare (sub n min_int) (sub m min_int)

(* Unsigned division from signed division of the same
   bitness. See Warren Jr., Henry S. (2013). Hacker's Delight (2 ed.), Sec 9-3.
*)
let div_unsigned n d =
  if d < zero then
    if compare_unsigned n d < 0 then zero else one
  else
    let q = shift_left (div (shift_right_logical n 1) d) 1 in
    let r = sub n (mul q d) in
    if compare_unsigned r d >= 0 then succ q else q

let rem_unsigned n d =
  sub n (mul (div_unsigned n d) d)
