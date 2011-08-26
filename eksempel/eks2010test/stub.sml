(* Opgave 1 *)
val fjern = stub

(* Opgave 2 *)
val udvaelg = stub
val udvaelg2 = stub

(* Opgave 3 *)
val sortPerm = stub

(* Opgave 4 *)
val visPoly = stub
val evalPoly = stub

(* Opgave 5 *)
val hentKalender = stub

(* Opgave 6 *)
datatype prop =
         VAR of string
       | NOT of prop
       | AND of prop * prop
       | OR of prop * prop
       | TT
       | FF
val eval = stub
val implies = stub
val simplify = stub

(* Opgave 7 *)
val klassedelinger = stub
val bell = stub

(* Opgave 8 *)
structure TrivKList =
struct
type 'a klist = unit
val tom = ()
val singleton = stub
val konkat = stub
val tilListe = stub
val laengde = stub
end
val fraListe = stub

(* Opgave 9 *)
val permNr = stub
