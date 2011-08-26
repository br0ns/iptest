val fjern =
    funktion2
      "fjern" fjern $
      char --> list char --> par (list char, int)

val udvaelg =
    funktion2
      "udvaelg" udvaelg $
      list char --> list int --> list char

val sortPerm =
    funktion
      "sortPerm" sortPerm $
      list real --> par (list int, list real)

val visPoly =
    funktion
      "visPoly" visPoly $
      list int --> string

val evalPoly =
    funktion2
      "evalPoly" evalPoly $
      list int --> int --> int

val hentKalender =
    funktion
    "hentKalender" hentKalender $
    string --> kalender

val eval =
    funktion2
      "eval" eval $
      prop --> tildeling --> bool

val implies =
    funktion
      "implies" implies $
      par (prop, prop) --> prop

val simplify =
    funktion
      "simplify" simplify $
      prop --> prop

val klassedelinger =
    funktion
      "klassedelinger" klassedelinger $
      list int --> list (list (list int))

val bell =
    funktion
      "bell" bell $
      int --> int

val singleton =
    funktion
      "TrivKList.singleton" TrivKList.singleton $
      int --> klist int

val konkat =
    funktion
      "TrivKList.konkat" TrivKList.konkat $
      par (klist int, klist int) --> klist int

val tilListe =
    funktion
      "TrivKList.tilListe" TrivKList.tilListe $
      klist int --> list int

val laengde =
    funktion
      "TrivKList.laengde" TrivKList.laengde $
      klist int --> int

val fraListe =
    funktion
      "fraListe" fraListe $
      list int --> klist int

val permNr =
    funktion2
      "permNr" permNr $
      int --> int --> list int
