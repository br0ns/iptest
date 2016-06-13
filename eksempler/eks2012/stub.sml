(* opgave 1*)
val erPalindrom = stub
val erUdvidetPalindrom = stub

(* opgave 2 *)
datatype rute = Stop | Frem of int * rute | Drej of int * rute
val korrekt = stub
val laengde = stub
val erNormaliseret = stub
val normaliserRute = stub

(* opgave 3 *)
val kvadratfrit = stub
val maksKvadratfrit = stub

(* opgave 4 *)
val erPermutationAf = stub
val antalPermutationer = stub
val antalPermutationerNy = stub

(* opgave 5 *)
val grupper = stub
val gentag = stub
val gcd = stub

(* opgave 6 *)
datatype 'a trae = K of 'a * ('a trae list)
val praeorden = stub
val erstat = stub
val sorter = stub

(* opgave 7 *)
val inverterPPM = stub

(* opgave 8 *)
signature SYMBOLTABEL =
sig
  type 'a tabel
  val tom     : 'a tabel
  val indsaet : 'a tabel -> (string * 'a) -> 'a tabel
  val find    : 'a tabel -> string -> 'a option
end

structure ListeTabel =
struct
  type 'a tabel = (string * 'a) list
  val tom     = []
  val indsaet = stub
  val find    = stub
end

structure FunTabel =
struct
  type 'a tabel = string -> 'a option
  val tom     = stub
  val indsaet = stub
  val find    = stub
end

