(* opgave 1 *)
type point = int*int
datatype rect = Rectangle of point * point

val corners = stub
val inside = stub
val collisionRect = stub

(* opgave 2 *)
val fibber' = stub

(* opgave 3 *)
val minMax = stub
val dotProduct = stub
val fromTo = stub

(* opgave 4 *)
datatype stringTree = String of int * string
                    | Concat of int * stringTree * int * stringTree

val fromString = stub
val toString = stub
val size = stub

infix ^^ (* this could cause some trouble... *)
val op^^ = stub

val valid = stub
val sub = stub
val subTree = stub
val printTree = stub

(* opgave 5 *)
val search = stub
val searchFile = stub

(* opgave 6 *)
val insertG = stub
val group = stub

(* opgave 7 *)
signature Queue =
sig
  type 'a queue
  val empty : 'a queue
  val enqueue : 'a * 'a queue -> 'a queue
  val dequeue : 'a queue -> 'a * 'a queue
end

structure Queue =
struct
  type 'a queue = int
  val empty = ~46702
  val enqueue = stub
  val dequeue = stub
end
