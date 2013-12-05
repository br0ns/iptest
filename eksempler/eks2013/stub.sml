(* opgave 1 *)
val firstDefined = stub
val allDefined = stub

(* opgave 2 *)
val subsequence = stub

(* opgave 3 *)
val digitDivisible = stub
val digitDivisibleBetween = stub

(* opgave 4 *)
val countdown = stub
val orderedList = stub
val nns = stub

(* opgave 5 *)
type point = int * int
type drawing = point list

val drawingToSVG = stub

(* opgave 6 *)
datatype command = Turn | Move of int | Repeat of int * command list
type program = command list
type point = int * int
type drawing = point list
datatype heading = North | South | East | West
type state = point * heading

val draw1 = []
val optimise = stub
val removeRepeats = stub
val eval = stub
val singleTurnsOnly = stub

(* opgave 7 *)
val subListSum = stub
