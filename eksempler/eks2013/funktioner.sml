(* opgave 1 *)
val firstDefined =
    funktion
      "firstDefined" firstDefined $
      list (option int) --> option int

val allDefined =
    funktion
      "allDefined" allDefined $
      list (option int) --> list int

(* opgave 2 *)
val subsequence =
    funktion
      "subsequence" subsequence $
      par (string, string) --> bool

(* opgave 3 *)
val digitDivisible =
    funktion
      "digitDivisible" digitDivisible $
      int --> bool

val digitDivisibleBetween =
    funktion
      "digitDivisibleBetween" digitDivisibleBetween $
      par (int, int) --> list int

(* opgave 4 *)
val countdown =
    funktion
      "countdown" countdown $
      int --> list int

val orderedList =
    funktion
      "orderedList" orderedList $
      list int --> bool

val nns =
    funktion
      "nns" nns $
      int --> list int

(* opgave 5 *)
(* Forsøger at normalisere en SVG, så den kan sammenlignes mod en referencefil*)
;use (Constants.TEST ^ "/normalizer.sml");
val drawingToSVG =
    funktion2
      "drawingToSVG" (fn fil => fn tegn =>
                        (drawingToSVG fil tegn; normaliserSVG fil)) $
      string & drawing --> unit

(* opgave 6 *)
val optimise =
    funktion
      "optimise" optimise $
      program --> program

val removeRepeats =
    funktion
      "removeRepeats" removeRepeats $
      program --> program

val eval =
    funktion
      "eval" eval $
      par(state, program) --> drawing

val singleTurnsOnly =
    funktion
      "singleTurnsOnly" singleTurnsOnly $
      program --> program

(* opgave 7 *)
val subListSum =
    funktion
      "subListSum" subListSum $
      par (int, list int) --> option (list int)
