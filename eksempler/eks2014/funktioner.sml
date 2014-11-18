;val _ = use (Constants.TEST ^ "/" ^ "vejl.sml");

(* opgave 1 *)
val corners =
  funktion
    "corners" corners $
    rect --> list point

val inside =
  funktion2
    "inside" inside $
    rect & point --> bool

val collisionRect =
  funktion2
    "collisionRect" collisionRect $
    rect & rect --> bool

(* opgave 2 *)
val fibber' =
  funktion
    "fibber'" fibber' $
    int --> int

(* opgave 3 *)
val minMax =
  funktion
    "minMax" minMax $
    list int --> par (int, int)

val dotProduct =
  funktion
    "dotProduct" dotProduct $
    par (list real, list real) --> real

val fromTo =
  funktion
    "fromTo" fromTo $
    par (int, int) --> list int

(* opgave 4 *)
val fromString =
  funktion
    "fromString" fromString $
    string --> stringTree

val toString =
  funktion
    "toString" toString $
    stringTree --> string

val size =
  funktion
    "size" size $
    stringTree --> int

nonfix ^^
val ^^ =
  funktion
    "^^" ^^ $
    par (stringTree, stringTree) --> stringTree

val valid =
  funktion
    "valid" valid $
    stringTree --> bool

val sub =
  funktion2
    "sub" sub $
    stringTree & int --> char

val subTree =
  funktion2
    "subTree" subTree $
    stringTree & par (int, int) --> stringTree

val printTreeFile =
  funktion
    "printTree" Vejl.printTreeFile $
    par (string, stringTree) --> unit

(* opgave 5 *)
val search =
  funktion2
    "search" search $
    string & string --> option int

val searchFile =
  funktion2
    "searchFile" searchFile $
    string & string --> option (par (int, int))

(* opgave 6 *)
val insertG =
  funktion
    "insertG" insertG $
    triple (int, int, list (list int)) --> list (list int)

val group =
  funktion2
    "group" group $
    func & list int --> list (list int)

(* opgave 7 *)
val queueFuncs =
  funktion
    "enqueue/dequeue" Vejl.doQueue $
    list queueop --> list int
