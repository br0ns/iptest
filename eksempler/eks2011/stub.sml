(* opgave 1 *)
val stirling = stub

(* opgave 2 *)
val erNedreTrekant = stub
val diagonal = stub
val vend = stub

(* opgave 3 *)
val f = stub
val g = stub
val h = stub

(* opgave 4 *)
val isMatrix = stub
val dim = stub
val transpose = stub
val multiply = stub

(* opgave 5 *)
val BWT = stub
val invBWT = stub

(* opgave 6 *)
datatype 'a btree = Blad | Gren of 'a btree * 'a * 'a btree
datatype 'a udvidelse = % | << | == of 'a | >>
val gendan = stub

(* opgave 7 *)
val wordNum = stub
val wc1 = stub
val display = stub
val wc = stub

(* opgave 8 *)
signature MONOID =
sig
  type t
  val mul : t * t -> t
  val one : t
end
structure StringConc =
struct
type t = string
val mul = stub
val one = ""
end
functor RepSquare (structure S : MONOID) =
struct
open S
val pow = stub
end
