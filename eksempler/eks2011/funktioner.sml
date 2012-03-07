(* opgave 1 *)
val stirling =
    funktion
      "stirling" stirling $
      par (int, int) --> int


(* opgave 2 *)
val trekant = list (list int)
val erNedreTrekant =
    funktion
      "erNedreTrekant" erNedreTrekant $
      trekant --> bool

val diagonal =
    funktion
      "diagonal" diagonal $
      trekant --> list int

val vend =
    funktion
      "vend" vend $
      trekant --> trekant


(* opgave 3 *)
val f =
    funktion2
      "f" f $
      list int & list int --> list int

val g =
    funktion2
      "g" g $
      list int & list pred --> list int

val h =
    funktion
      "h" h $
      list real --> par (real, real)


(* opgave 4 *)
val matrix = list (list int)
val isMatrix =
    funktion
      "isMatrix" isMatrix $
      matrix --> bool

val dim =
    funktion
      "dim" dim $
      matrix --> par (int, int)

val transpose =
    funktion
      "transpose" transpose $
      matrix --> matrix

val multiply =
    funktion
      "multiply" multiply $
      par (matrix, matrix) --> matrix


(* opgave 5 *)
val BWT =
    funktion
      "BWT" BWT $
      string --> string

val invBWT =
    funktion
      "invBWT" invBWT $
      par (string, char) --> string


(* opgave 6 *)
val gendan =
    funktion
      "gendan" gendan $
      list (udvidelse string) --> btree string


(* opgave 7 *)
val wordNum =
    funktion
      "wordNum" wordNum $
      string --> int

val wc1 =
    funktion
      "wc1" wc1 $
      string --> triple (int, int, int)

val display =
    funktion2
      "display" display $
      int & int --> string

val wc =
 fn filnavn =>
    funktion
      "wc" wc $
      par (list string, option string) --> fil filnavn


(* opgave 8 *)
val pow =
    funktion
      "StringPow.pow" StringPow.pow $
      par (string, int) --> string
