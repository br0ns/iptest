(* opgave 1 *)
;stirling : int * int -> int;

(* opgave 2 *)
;erNedreTrekant : 'a list list -> bool;
;diagonal : 'a list list -> 'a list;
;vend : 'a list list -> 'a list list;

(* opgave 3 *)
;f : 'a list -> int list -> 'a list;
;g : 'a list -> ('a -> bool) list -> 'a list;
;h : real list -> real * real;

(* opgave 4 *)
type 'a matrix = 'a list list
;isMatrix : 'a matrix -> bool;
;dim : 'a matrix -> int * int
;transpose : 'a matrix -> 'a matrix;
;multiply : int matrix * int matrix -> int matrix;

(* opgave 5 *)
;BWT : string -> string;
;invBWT : string * char -> string;

(* opgave 6 *)
;gendan : 'a udvidelse list -> 'a btree;

(* opgave 7 *)
;wordNum : string -> int;
;wc1 : string -> int * int * int;
;display : int -> int -> string;
;wc : string list * string option -> unit;

(* opgave 8 *)
signature MONOIDWPOW =
sig
  include MONOID
  val pow : t * int -> t
end

;structure FOO = StringConc : MONOID;
;structure StringPow = RepSquare(structure S = StringConc)
                       : MONOIDWPOW where type t = string;
