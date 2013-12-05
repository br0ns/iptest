(* opgave 1 *)
;firstDefined : 'a option list -> 'a option;
;allDefined : 'a option list -> 'a list;

(* opgave 2 *)
;subsequence : string * string -> bool;

(* opgave 3 *)
;digitDivisible : int -> bool;
;digitDivisibleBetween : int * int -> int list;

(* opgave 4 *)
;countdown : int -> int list;
;orderedList : int list -> bool;
;nns : int -> int list;

(* opgave 5 *)
;drawingToSVG : string -> drawing -> unit;

(* opgave 6 *)
;draw1 : program;
;optimise : program -> program;
;removeRepeats : program -> program;
;eval : state * program -> drawing;
;singleTurnsOnly : program -> program;

(* opgave 7 *)
;subListSum : int * int list -> int list option;
