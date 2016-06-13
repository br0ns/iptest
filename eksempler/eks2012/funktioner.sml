(* opgave 1 *)
val erPalindrom =
    funktion
      "erPalindrom" erPalindrom $
      string --> bool

val erUdvidetPalindrom =
    funktion
      "erUdvidetPalindrom" erUdvidetPalindrom $
      string --> bool

(* opgave 2 *)
val korrekt =
    funktion
      "korrekt" korrekt $
      rute --> bool

val laengde =
    funktion
      "laengde" laengde $
      rute --> int

val erNormaliseret =
    funktion
      "erNormaliseret" erNormaliseret $
      rute --> bool

val normaliserRute =
    funktion
      "normaliserRute" normaliserRute $
      rute --> rute

(* opgave 3 *)
val kvadratfrit =
    funktion
      "kvadratfrit" kvadratfrit $
      int --> bool

val maksKvadratfrit =
    funktion
      "maksKvadratfrit" maksKvadratfrit $
      int --> int

(* opgave 4 *)
val erPermutationAf =
    funktion
      "erPermutationAf" erPermutationAf $
      par (list int, list int) --> bool

val antalPermutationer =
    funktion
      "antalPermutationer" antalPermutationer $
      list int --> int

val antalPermutationerNy =
    funktion
      "antalPermutationerNy" antalPermutationerNy $
      list int --> int

(* opgave 5 *)
val grupper =
    funktion2
      "grupper" grupper $
      func & list int --> list(list int)

val gentag =
    funktion2
      "gentag" gentag $
      func & list int --> list int

val gcd =
    funktion
      "gcd" gcd $
      par (int, int) --> int

(* opgave 6 *)
val praeorden =
    funktion
      "praeorden" praeorden $
      trae int --> list int

val erstat =
    funktion
      "erstat" erstat $
      par (trae int, list int) --> par (trae int, list int)

val sorter =
    funktion
      "sorter" sorter $
      trae int --> trae int

(* opgave 7 *)
val inverterPPM =
    funktion2
      "inverterPPM" inverterPPM $
      string & string --> unit

(* opgave 8 *)
;val _ = use (Constants.TEST ^ "/" ^ "vejl.sml");

val listeIndsaet =
    funktion
      "ListeTabel.indsaet" Vejl.makeLT $
      list (par (string, int)) --> listetabel int

val funIndsaet =
    funktion
      "FunTabel.indsaet" Vejl.makeFT $
      list (par (string, int)) --> funtabel int
