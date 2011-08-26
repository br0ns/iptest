;load "Listsort";

(* Opgave 1 *)
fun fjern x (y :: ys) =
    if x = y then
      (ys, 0)
    else
      let
        val (ys', n) = fjern x ys
      in
        (y :: ys', n + 1)
      end

(* opgave b:
 * x må ikke forekomme forud for plads n i xr
 *)


(* Opgave 2 *)
fun curry f x y = f (x, y)
val udvaelg2 = map o curry List.nth
val udvaelg = udvaelg2


(* Opgave 3 *)
fun nummerer xs =
    let
      fun loop _ nil = nil
        | loop n (x :: xs) = (n, x) :: loop (n + 1) xs
    in
      loop 0 xs
    end
fun sammenlign ((_, x), (_, y)) = Real.compare (x, y)

fun sortPerm xs =
    (ListPair.unzip o Listsort.sort sammenlign o nummerer) xs

(* Listsort.sort kører i O(n log n) tid *)


(* Opgave 4 *)
fun evalPoly ys x =
    let
      fun loop nil = 0
        | loop (y :: ys) = y + x * loop ys
    in
      loop (rev ys)
    end

fun visKonstant a =
    case Int.compare (a, 0) of
      LESS => "-" ^ Int.toString (~a)
    | EQUAL => ""
    | GREATER => "+" ^ Int.toString a

fun visFaktor a =
    if abs a <> 1 then
      Int.toString (abs a)
    else
      ""

fun visEksponent n =
    if n > 1 then
      "^" ^ Int.toString n
    else
      ""

fun visLed (0, _) = ""
  | visLed (a, n) = visFaktor a ^ "x" ^ visEksponent n

fun visPoly nil = "0"
  | visPoly [y] = if y < 0 then
                    "-" ^ Int.toString (~y)
                  else
                    Int.toString y
  | visPoly ys =
    let
      fun loop _ nil = ""
        | loop n [y] = if y > 0 then
                         visLed (y, n)
                       else
                         "-" ^ visLed (y, n)
        | loop n (y :: ys) = loop (n + 1) ys ^
                             (case Int.compare (y, 0) of
                                LESS => "-" ^ visLed (y, n)
                              | EQUAL => ""
                              | GREATER => "+" ^ visLed (y, n)
                             )
      fun vis (y :: ys) = loop 1 ys ^ visKonstant y
    in
      vis (rev ys)
    end


(* Opgave 5 *)
fun hentfil s =
    let
      val is = TextIO.openIn s
    in
      TextIO.inputAll is before TextIO.closeIn is
    end

fun linje l =
    let
      val (d :: m :: a :: t) = String.tokens Char.isSpace l
      fun tal s = valOf (Int.fromString s)
      fun aar a = if a < 99 then
                    if a > 20 then
                      1900 + a
                    else
                      2000 + a
                  else
                    a
      fun saml nil = ""
        | saml [x] = x
        | saml (x :: xs) = x ^ " " ^ saml xs
    in
      SOME ((tal d, tal m, aar (tal a)), saml t)
    end handle _ => NONE

fun sammenlign (x as ((d, m, a), _), y as ((d', m', a'), _)) =
    case (Int.compare (a, a'), Int.compare (m, m'), Int.compare (d, d')) of
      (GREATER, _, _) => GREATER
    | (EQUAL, GREATER, _) => GREATER
    | (EQUAL, EQUAL, GREATER) => GREATER
    | (LESS, _, _) => LESS
    | (EQUAL, LESS, _) => LESS
    | (EQUAL, EQUAL, LESS) => LESS
    | _ => EQUAL

fun hentKalender k =
    (Listsort.sort sammenlign o List.mapPartial linje o String.tokens (fn c => c = #"\n") o hentfil) k


(* Opgave 6 *)
datatype prop
  = VAR of string
  | NOT of prop
  | AND of prop * prop
  | OR of prop * prop
  | TT
  | FF

fun eval p t =
    case p of
      VAR s => t s
    | NOT p => not (eval p t)
    | AND (p1, p2) => eval p1 t andalso eval p2 t
    | OR (p1, p2) => eval p1 t orelse eval p2 t
    | TT => true
    | FF => false

fun implies (p, c) = OR (NOT p, c)

fun simplify p =
    case p of
      NOT p =>
      (case simplify p of
         TT => FF
       | FF => TT
       | NOT p => p
       | p => NOT p)
    | AND (p1, p2) =>
      (case (simplify p1, simplify p2) of
         (TT, p) => p
       | (p, TT) => p
       | (FF, _) => FF
       | (_, FF) => FF
       | ps => AND ps)
    | OR (p1, p2) =>
      (case (simplify p1, simplify p2) of
         (FF, p) => p
       | (p, FF) => p
       | (TT, _) => TT
       | (_, TT) => TT
       | ps => OR ps)
    | _ => p


(* Opgave 7 *)
fun foranAlle _ nil = nil
  | foranAlle x (y :: ys) = (x :: y) :: foranAlle x ys

fun klassedelinger nil = [nil]
  | klassedelinger (x :: xs) =
    let
      fun loop nil = nil
        | loop (k :: ks) = ((x :: k) :: ks) ::
                           foranAlle k (loop ks)
      val k = klassedelinger xs
    in
      foranAlle [x] k @ List.concat (map loop k)
    end


fun faldende _ 0 = 1
  | faldende n k = n * faldende (n - 1) (k - 1)

fun fak 0 = 1
  | fak n = n * fak (n - 1)

fun binom (n, k) = faldende n (n - k) div fak (n - k)
fun binom (n, k) = fak n div (fak k * fak (n - k))

fun bell 0 = 1
  | bell n =
    let
      fun loop 0 = [1]
        | loop n =
          let
            val bs = loop (n - 1)
            val bsr = rev bs
          in
            foldl op+ 0 (List.tabulate (n, fn k => binom (n - 1, k) * List.nth (bsr, k))) :: bs
          end
    in
      hd (loop n)
    end

fun tjek (n, m) = bell (n + m) mod m = (bell (n + 1) + bell n) mod m
(* modeksempel: *)
(* n = 2, m = 6 *)


(* Opgave 8 *)

signature KLIST =
sig
(* Typen af endelige k-lister med elementer af type ’a *)
type 'a klist
(* Den tomme k-liste *)
val tom : 'a klist
(* Et-elements k-liste *)
val singleton : 'a -> 'a klist
(* Konkateneringen af to k-lister *)
val konkat : 'a klist * 'a klist -> 'a klist
(* Omsætning til almindelig liste *)
val tilListe : 'a klist -> 'a list
(* Længden af en k-liste, det vil sige antallet af elementer i den *)
val laengde : 'a klist -> int
end

structure TrivKList :> KLIST =
struct
type 'a klist = 'a list
val tom = nil
fun singleton x = [x]
val konkat = op@
fun tilListe x = x
val laengde = length
end

fun fraListe xs =
    let open TrivKList in
      foldr konkat tom o map singleton
    end xs

structure KList :> KLIST =
struct
datatype 'a klist' = S of 'a
                   | K of 'a klist' * 'a klist'
type 'a klist = ('a klist' * int) option
val tom = NONE
fun singleton x = SOME (S x, 1)
fun konkat (SOME (x, n), SOME (y, m)) = SOME (K (x, y), n + m)
  | konkat (NONE, y) = y
  | konkat (x, NONE) = x
  | konkat _ = NONE
fun laengde NONE = 0
  | laengde (SOME (_, n)) = n
fun tilListe NONE = nil
  | tilListe (SOME (k, _)) =
    let
      fun loop (S x) xs = x :: xs
        | loop (K (x, y)) xs = loop x (loop y xs)
    in
      loop k nil
    end
end


(* Opgave 9 *)
fun indsaet x (xr, n) = List.take (xr, n) @ x :: List.drop (xr, n)

fun permNr 0 _ = []
  | permNr n k =
    let
      val xs = permNr (n - 1) (k div n)
    in
      indsaet (n - 1) (xs, k mod n)
    end
