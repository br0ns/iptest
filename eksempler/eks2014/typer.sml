(* opgave 1 *)
;corners : rect -> point list;
;inside : rect -> point -> bool;
;collisionRect : rect -> rect -> bool;

(* opgave 2 *)
;fibber' : int -> int;

(* opgave 3 *)
;minMax : int list -> int * int;
;dotProduct : real list * real list -> real;
;fromTo : int * int -> int list;

(* opgave 4 *)
;fromString : string -> stringTree;
;toString : stringTree -> string;
;size : stringTree -> int;
;op^^ : stringTree * stringTree -> stringTree;
;valid : stringTree -> bool;
;sub : stringTree -> int -> char;
;subTree : stringTree -> int * int -> stringTree;
;printTree : TextIO.outstream * stringTree -> unit;

(* opgave 5 *)
;search : string -> string -> int option;
;searchFile : string -> string -> (int * int) option;

(* opgave 6 *)
;insertG : int * 'a * 'a list list -> 'a list list;
;group : ('a -> int) -> 'a list -> 'a list list;

(* opgave 7 *)
;structure FOO = Queue : Queue;
;Queue.empty : 'a Queue.queue;
;Queue.enqueue : 'a * 'a Queue.queue -> 'a Queue.queue;
;Queue.dequeue : 'a Queue.queue -> 'a * 'a Queue.queue;

(* for constructing testcases *)
datatype queueop = Enqueue of int
                 | Dequeue
