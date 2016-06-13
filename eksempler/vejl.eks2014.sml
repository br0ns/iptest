(* IP-eksamen 2014, vejledende løsning *)

(* højreassociativ funktionsanvendelse *)
infixr $
fun f $ x = f x

(* Opgave 1 *)
type point = int * int
datatype rect = Rectangle of point * point

(* corners : rect -> point list
 * Finder hjørnerne i et rektangel *)
fun corners (Rectangle ((x1, y1), (x2, y2))) =
    [ (x1, y1), (x1, y2), (x2, y1), (x2, y2) ]

(* inside : rect -> point -> bool
 * Er punktet inde i rektanglet? *)
fun inside (Rectangle ((x1, y1), (x2, y2))) (x, y) =
    x1 <= x andalso x <= x2 andalso y1 <= y andalso y <= y2

(* collisionRect : rect -> rect -> bool
 * Kolliderer de to rektangler? *)
(* Denne er fristende, men ej korrekt:
 
fun collisionRect r1 r2 =
    List.exists (inside r1) (corners r2) orelse
    List.exists (inside r2) (corners r1)

   Overvej følgende case:

      o-----o
      |     |
  o---+-----+---o
  |             |
  o---+-----+---o
      |     | 
      o-----o

   I stedet finder vi ud af hvor de ikke kan overlappe, og negerer dette.
*)
fun collisionRect (Rectangle ((flx, fly), (frx, fry)))
                  (Rectangle ((slx, sly), (srx, sry))) =
    not (frx < slx orelse    (* B fuldstændigt til højre for A *)
         flx > srx orelse    (* B fuldstændigt til venstre for A *)
         fry < sly orelse    (* B fuldstændigt over A *)
         fly > sry)          (* B fuldstændigt under A *)


(* Opgave 2 *)
local
  (* fibber_it beregner iterativt fibber *)
  fun fibber_it p0 _ _ 0 = p0
    | fibber_it p0 p1 p2 n = fibber_it p1 p2 (p0 + p2) (n-1)
in
  (* fibber' : int -> int
   * Beregner fibber-følgen *)
  fun fibber' n = fibber_it 0 1 2 n
end

(* Opgave 3 *)

(* minMax : int list -> int * int
 * Beregner minimum og maksimum af en liste *)
fun minMax []        = raise Empty
  | minMax (x :: xs) =
  foldl (fn (e, (l, h)) => (Int.min(e, l), Int.max(e, h))) (x, x) xs

;load "ListPair";

(* dotProduct : real list * real list -> real
 * Beregner prikproduktet af to lister *)
val dotProduct = ListPair.foldl (fn (x, y, a) => x*y + a) 0.0

(* fromTo : int * int -> int list
 * fromTo (m, n) giver listen af tal fra m -> n *)
fun fromTo (m, n) =
  if m < n then List.tabulate (n - m + 1, fn i => m + i)
  else          List.tabulate (m - n + 1, fn i => m - i)

(* Opgave 4 *)
datatype stringTree = String of int * string
                    | Concat of int * stringTree * int * stringTree

(* fromString : string -> stringTree
 * Laver et stringTree ud af en string *)
fun fromString s = String (String.size s, s)

(* toString : stringTree -> string
 * Fladgør et stringTree til en streng *)
fun toString (String (_, s)) = s
  | toString (Concat (_, s, _, t)) = toString s ^ toString t

(* size : stringTree -> int
 * Giver længden på en tekst *)
fun size (String (n, _)) = n
  | size (Concat (n, _, m, _)) = n + m

(* ^^ : stringTree * stringTree -> stringTree
 * Konkatenerer to strenge *)
infixr 6 ^^
fun s ^^ t = Concat (size s, s, size t, t)

(* valid : stringTree -> bool
 * Er det givne træ gyldigt? *)
fun valid (String (n, s)) = n = String.size s
  | valid (Concat (n, s, m, t)) =
    n = size s andalso m = size t andalso valid s andalso valid t

(* sub : stringTree -> int -> char
 * sub s i udtrækker det i'te tegn i s *)
fun sub (String (_, s)) i = String.sub (s, i)
  | sub (Concat (n, s, m, t)) i =
    if i < n then sub s i else sub t (i - n)

(* subTree : stringTree -> int * int -> stringTree
 * subTree s (i, l) udtrækker delstrengen s[i..i+l-1] *)
fun subTree (String (_, s)) (i, l) = String (l, String.substring (s, i, l))
  | subTree (Concat (n, s, m, t)) (i, l) =
  if i + l <= n (* fuldt indeholdt i s *)
  then subTree s (i, l)
  else if i >= n (* fuldt indeholdt i t *)
  then subTree t (i-n, l)
  else let val r = n - i
       in Concat (r, subTree s (i, r), l-r, subTree t (0, l - r))
       end

(* printTree : TextIO.outstream * stringTree -> unit
 * Udskriver en tekst til en strøm *)
fun printTree (os, String (_, s)) = TextIO.output (os, s)
  | printTree (os, Concat (_, s, _, t)) = (printTree (os, s); printTree (os, t))

(* Opgave 5 *)
(* search : string -> string -> int option
 * search key value giver placeringen af key i value *)
fun search key value =
  if not $ String.isSubstring key value then
    NONE
  else
    SOME $ Substring.size $ #1 $ Substring.position key (Substring.full value)

(* searchFile : string -> string -> (int * int) option
 * searchFile key file giver linje/søjle på første forekomst af key i filen
 * file *)
local
  fun findLine key is i =
    case TextIO.inputLine is of
        NONE   => NONE
      | SOME l =>
          case search key l of
              NONE   => findLine key is (i+1)
            | SOME p => SOME (i, p)
in
  fun searchFile key file =
    let val is = TextIO.openIn file
    in findLine key is 1
       before TextIO.closeIn is
    end
end

(* Opgave 6 *)
(* insertG : int * 'a * 'a list list -> 'a list list
 * insertG (i, x, xs) indsætter x i den i'te liste i xs *)
fun insertG (0, x, [])    = [[x]]
  | insertG (0, x, y::ys) = (x :: y) :: ys
  | insertG (n, x, [])    = if n < 0 then raise Domain
                                     else [] :: insertG (n-1, x, [])
  | insertG (n, x, y::ys) = if n < 0 then raise Domain
                                     else y :: insertG (n-1, x, ys)

(* group : ('a -> int) -> 'a list -> 'a list list
 * Grupperer elementer efter et prædikat *)
fun group p = List.foldl (fn (e, a) => insertG (p e, e, a)) []

(* Opgave 7 *)
signature Queue =
sig
    (* Typen af en kø med elementer af typen 'a. *)
    type 'a queue

    (* Den tomme kø *)
    val empty : 'a queue

    (* Tilføj et element bagerst i køen. *)
    val enqueue : 'a * 'a queue -> 'a queue

    (* Tag et element ud forrest fra køen.
     * Hejser Empty hvis køen er tom. *)
    val dequeue : 'a queue -> 'a * 'a queue
end

structure Queue :> Queue =
struct
  (* Vores kø er repræsenteret ved to lister, (xs, ys).
   * xs er forenden af køen, og ys er bagenden.
   * Dvs, repræsenteret som en liste svarer (xs, ys) til køen xs @ rev ys. *)
  type 'a queue = 'a list * 'a list

  val empty = ([], [])

  (* indsæt i O(1) *)
  fun enqueue (x, (xs, ys)) = (xs, x::ys)

  (* udtræk i amortiseret O(1) *)
  fun dequeue ([], [])    = raise Empty
    | dequeue (x::xs, ys) = (x, (xs, ys))
    | dequeue ([], ys)    = dequeue (rev ys, [])
end
