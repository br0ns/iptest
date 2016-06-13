(* opgave 1 *)
;erPalindrom : string -> bool;
;erUdvidetPalindrom : string -> bool;

(* opgave 2 *)
;korrekt : rute -> bool;
;laengde : rute -> int;
;erNormaliseret : rute -> bool;
;normaliserRute : rute -> rute;

(* opgave 3 *)
;kvadratfrit : int -> bool;
;maksKvadratfrit : int -> int;

(* opgave 4 *)
;erPermutationAf : ''a list * ''a list -> bool;
;antalPermutationer : ''a list -> int;
;antalPermutationerNy : ''a list -> int;

(* opgave 5 *)
;grupper : ('a -> ''b) -> 'a list -> 'a list list;
;gentag : ('a -> 'a) -> 'a -> 'a;
;gcd : int * int -> int;

(* opgave 6 *)
;praeorden : 'a trae -> 'a list;
;erstat : 'a trae * 'b list -> 'b trae * 'b list;
;sorter : int trae -> int trae;

(* opgave 7 *)
;inverterPPM : string -> string -> unit;

(* opgave 8 *)
;structure FOO = ListeTabel : SYMBOLTABEL;
;structure BAR = FunTabel : SYMBOLTABEL;
