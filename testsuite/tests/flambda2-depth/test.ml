(* TEST
* flambda
*)

external ( - ) : int -> int -> int = "%subint"
external ( * ) : int -> int -> int = "%mulint"

let rec fact_plain = function
  | 0 -> 1
  | n -> n * fact_plain (n - 1)
[@@inline] (* [@unrolled] doesn't work without this *)

let test1 () = (fact_plain [@unrolled 3]) 6

let () = Format.printf "%d\n" (test1 ())

let rec fact_unrolled = function
  | 0 -> 1
  | n -> n * (fact_unrolled [@unrolled 3]) (n - 1)
[@@inline]

let test2 () = fact_unrolled 3
let test3 () = fact_unrolled 20
let test4 () = (fact_unrolled [@unrolled 4]) 6
let test5 () = ((fact_unrolled [@unrolled 7]) 30)
