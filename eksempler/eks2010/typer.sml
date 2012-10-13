(* Opgave 1 *)
;fjern : ''a -> ''a list -> ''a list * int;

(* Opgave 2 *)
;udvaelg : char list -> int list -> char list;
;udvaelg2 : char list -> int list -> char list;

(* Opgave 3 *)
;sortPerm : real list -> int list * real list;

(* Opgave 4 *)
;visPoly : int list -> string;
;evalPoly : int list -> int -> int;

(* Opgave 5 *)
;hentKalender : string -> ((int * int * int) * string) list;

(* Opgave 6 *)
;eval : prop -> (string -> bool) -> bool;
;implies : prop * prop -> prop;
;simplify : prop -> prop;

(* Opgave 7 *)
;klassedelinger : 'a list -> 'a list list list;
;bell : int -> int;

(* Opgave 8 *)
signature KLIST =
sig
  type 'a klist
  val tom : 'a klist
  val singleton : 'a -> 'a klist
  val konkat : 'a klist * 'a klist -> 'a klist
  val tilListe : 'a klist -> 'a list
  val laengde : 'a klist -> int
end
;structure FOO = TrivKList :> KLIST;
;fraListe : 'a list -> 'a TrivKList.klist;

(* Opgave 9 *)
;permNr : int -> int -> int list;
