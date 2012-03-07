             (* Introduktion til Programmering *)
             (* DIKU, efteråret 2011           *)
             (* Vejledende løsning af eksamen  *)


(* Jeg starter med at erklaere nogle hjaelpefunktioner jeg tit bruger
 * `$` kender vi fra uge 4.
 *)

nonfix $
fun $ (f, x) = f x
infixr $
fun curry f x y = f (x, y)
fun uncurry f (x, y) = f x y
fun const x _ = x

(* Opgave 1a
 * Den rekursive definition af stirlingtallet af anden art er gengivet med
 * funktionen `loop`. Ved at bruge en lokal funktion kan vi undgaa at lave
 * robusthedstjekket for hvert rekursive kald.
 *)
fun stirling (n, k) =
    let
      fun loop (0, 0) = 1
        | loop (_, 0) = 0
        | loop (0, _) = 0
        | loop (n, k) = k * loop (n - 1, k) + loop (n - 1, k - 1)
    in
      if n < 0 orelse k < 0
      then raise Domain
      else loop (n, k)
    end

(* Opgave 1b
 *)
(* stirling (4, 2) ->
 *
 * if 4 < 0 orelse 2 < 0 then raise Domain else loop (4, 2)
 *
 * loop (4, 2)
 *
 * 2 * loop (3, 2) + loop (3, 1)
 *
 * 2 * (2 * loop (2, 2) + loop (2, 1)) +
 *     (1 * loop (2, 1) + loop (2, 0))
 *
 * 2 * (2 * (2 * loop (1, 2) + loop (1, 1)) +
 *          (1 * loop (1, 1) + loop (1, 0))) +
 *     (1 * (1 * loop (1, 1) + loop (1, 0)) +
 *           0)
 *
 * 2 * (2 * (2 * (2 * loop (0, 2) + loop (0, 1)) +
 *               (1 * loop (0, 1) + loop (0, 0))) +
 *          (1 * (1 * loop (0, 1) + loop (0, 0)) +
  *               0)) +
 *     (1 * (1 * (1 * loop (0, 1) + loop (0, 0)) +
  *               0) +
             0)
 *
 * 2 * (2 * (2 * (2 * 0 + 0) +
 *               (1 * 0 + 1)) +
 *          (1 * (1 * 0 + 1) +
  *               0)) +
 *     (1 * (1 * (1 * 0 + 1) +
 *                0) +
             0)
 *
 * 2 * (2 * (2 * 0 +
 *               1) +
 *          (1 * 1 +
 *               0)) +
 *     (1 * (1 * 1 +
 *               0) +
             0)
 *
 * 2 * (2 * (0 + 1) +
 *          (1 + 0)) +
 *     (1 * (1 + 0) +
             0)
 *
 * 2 * (2 * 1 + 1) + 1
 *
 * 2 * 3 + 1
 *
 * 7
 *)

(* stirling (~3, 0)
 *
 * if ~3 < 0 orelse 0 < 0 then raise Domain else loop (~3, 0)
 *
 * raise Domain
 *
 * unhandled exception Domain
 *)

(* stirling (~2, ~1)
 *
 * if ~2 < 0 orelse ~1 < 0 then raise Domain else loop (~2, ~1)
 *
 * unhandled exception Domain
 *)

(* stirling (30, 28)
 *
 * "left as an exercise for the reader"
 *)


(* Opgave 2a
 * Hjaelpefunktionen undersoeger om den foerste liste har laengde `n` den, anden
   laenge `n + 1` osv.
 *)
fun erNedreTrekant xs =
    let
      fun loop n nil = true
        | loop n (x :: xs) = length x = n andalso loop (n + 1) xs
    in
      loop 1 xs
    end

(* Opgave 2b
 * Diagonalen af en trekant er netop det sidste element i hver af listerne.
 *)
fun diagonal xs = map List.last xs

(* Opgave 2c
 * En trekant kan vendes ved at beskaere foerste element af alle listerne og
 * derefter vende resten -- paanear den foerste liste som jo nu er tom --
 * rekursivt.
 *)
fun vend nil = nil
  | vend (xs as _ :: xs') = map hd xs :: (vend $ map tl xs')


(* Opgave 3a-c
 *)

(* Kaldet `f xs ns` returnerer listen bestaaende af elementerne paa plads `n`
 * (et-indekseret) for alle `n` i `ns`. Raekkefoelgen er givet af `n`'erne i
 * `ns`
 *)
fun f xs ns = map (fn n => List.nth (xs, n - 1)) ns

(* Kaldet `g xs fs` returnerer listen af de `x`'er i `xs` som opfylder samtlige
 * praedikater `f i `fs`. Raekkefoelgen er uaendret
 *)
fun both (f, g) x = f x andalso g x
fun g xs fs = List.filter (foldl both (const true) fs) xs
(* Eller *)
fun g xs fs = foldl (fn (f, xs) => List.filter f xs) xs fs
(* Eller (`uncurry` er defineret i toppen af filen) *)
fun g xs fs = foldl (uncurry List.filter) xs fs

(* Kaldet `h xs` returnerer paret `(min, max)` hvor `min` er et mindste `x i
 * `xs` og `max` er et stoerste `x` i `xs`. Hvis `xs` er den tomme liste kastes
 * `Match`
 *)
fun h (x :: xs) = foldl (fn (x, (min, max)) =>
                            (Real.min (x, min), Real.max (x, max))
                        ) (x, x) xs


(* Opgave 3d
 * f : Koeretiden er O(n^2)
 *     For hvert index i `upto (1, n)` skal vi laengere og laengere ned i `xs`.
 *
 * h : Koeretiden er O(n)
 *     Der bruges en venstrefoldning, saa hvert element behandles kun een gang.
 *)


(* Opgave 4a
 * Husk at en matrix har mindst laengde 1 paa hver led.
 *)
fun isMatrix nil = false
  | isMatrix (x :: xs) =
    let
      val l = length x
    in
      l > 0 andalso
      List.all (fn y => length y = l) xs
    end

(* Opgave 4b
 * Listen kan antages ikke-tom da den repraesenterer en matrix.
 *)
fun dim xs = (length xs, length $ hd xs)

(* Opgave 4c
 * `transpose` fjerner en soejle af gangen, og koerer rekursivt paa resten.
 * `null` er en biblioteksfunktion som undersoeger om en liste er tom.
 *)
fun transpose xs =
    if null $ hd xs
    then nil
    else map hd xs :: (transpose $ map tl xs)

(* Opgave 4d
 * `row` udregner en raekke ved at gange en given raekke (fra `xs`) sammen med
 * samtlige soejler i `ys`.
 * Det er vigtigt at undersoege om dimensionerne passer da `ListPair.zip`
 * beskaerer den laengste liste.
 *)
fun multiply (xs, ys) =
    let
      val rowmul = foldl op+ 0 o map op* o ListPair.zip
      fun row x = map (curry rowmul x) $ transpose ys
    in
      if #2 (dim xs) = #1 (dim ys)
      then map row xs
      else raise Domain
    end


(* Opgave 5a
 *)
;load "Listsort";

(* `collate` kan givet en sammenligningsfunktion for en type elementer,
 * sammenligne lister af den type
 *)
fun collate _ (nil, nil) = EQUAL
  | collate _ (nil, _) = LESS
  | collate _ (_, nil) = GREATER
  | collate cmp (x :: xs, y :: ys) =
    case cmp (x, y) of
      EQUAL => collate cmp (xs, ys)
    | x     => x

(* `rot` giver samtlige rotationer af en tekst. *)
fun BWT s =
    let
      fun rot xs =
          let
            fun loop nil xs = nil
              | loop (x :: xs) ys =
                let val ys' = x :: ys
                in (xs @ rev ys') :: loop xs ys'
                end
          in
            loop xs nil
          end
      val sort = Listsort.sort (collate Char.compare)
    in
      implode $ map List.last $ sort $ rot $ explode s
    end

(* Opgave 5b
 * Paa sit `n`te gennemloeb returnerer `loop` den oenskede tekst.
 *)
fun invBWT (t, last) =
    let
      val t = explode t
      val sort = Listsort.sort (collate Char.compare)
      fun loop 1 ss = hd $ List.filter (fn s => List.last s = last) ss
        | loop n ss =
          loop (n - 1) $ map op :: $ ListPair.zip (t, sort ss)
    in
      implode $ loop (length t) $ map (fn x => [x]) t
    end


(* Opgave 6
 * Denne opgave handler om invarianter. Fra opgaven ved vi at inddata er
 * velformet, altsaa det repraesenterer et trae.
 *)
datatype 'a btree = Blad | Gren of 'a btree * 'a * 'a btree
datatype 'a udvidelse = % | << | == of 'a | >>

fun lineariser Blad = [%]
  | lineariser (Gren (t1, x, t2)) =
    << :: lineariser t1 @ == x :: lineariser t2 @ [>>]

val udtr = Gren (Gren (Gren (Blad,"10",Blad),"-",Gren (Blad,"6",Blad)),
                 "*",
                 Gren (Blad,"2",Blad))

(* Funktionen `loop` overholder en invariant at den udtraekker netop et trae fra
 * sit input og returnerer traet og resten af inputtet som et par.
 * Da input antages at repraesentere et trae maa det foerste symbol vaere `%`
 * eller `<<`. I faldet `%` er vi faerdige med det samme, ellers ved vi per
 * invariant at der kommer et trae, symbolet `==`, et trae og symbolet `>>`. Saa
 * de fire ting udtraekkes med to rekursive kald, og vi er faerdige.
 *
 * Til sidst returneres det indlaeste trae og overskydende indata (som er den
 * tomme liste) smides bort.
 *)
fun gendan xs =
    let
      fun loop (% :: xs) = (Blad, xs)
        | loop (<< :: xs) =
          let
            val (t1, == x :: xs) = loop xs
            val (t2, >> :: xs) = loop xs
          in
            (Gren (t1, x, t2), xs)
          end
    in
      #1 $ loop xs
    end


(* Opgave 7a
 * `String.tokens` bryder paa en eller flere forekomster af et tegn som opfylder
 * praedikatet.
 * `Char.isSpace` returnerer `true` for alle slags blanktegn.
 *)
val wordNum = length o String.tokens Char.isSpace

(* Opgave 7b
 * Vi husker at lukke filstroemme
 *)
fun wc1 file =
    let
      val is = TextIO.openIn file
      val s = TextIO.inputAll is
      val _ = TextIO.closeIn is
      val ls = String.fields (fn c => c = #"\n") s
      val sum = foldl op+ 0
    in
      (if List.last ls = ""
       then length ls - 1
       else length ls
     , sum $ map wordNum ls
     , size s)
    end

(* Opgave 7c
 *)
fun display padding n =
    let
      fun spaces n = implode $ List.tabulate (n, const #" ")
      fun leftpad n s =
          let
            val pad = n - size s
          in
            if pad > 0
            then spaces pad ^ s
            else s
          end
    in
      leftpad padding $ Int.toString n
    end

(* Opgave 7d
 *)
fun wc (files, dst) =
    let
      (* Stroemmen vi skal skrive til og en funktion til at lukke den. Hvis vi
       * skriver itl `stdOut` er lukkefunktionen bare en pladsholder som ikke
       * goer noget.
       *)
      val (outs, close) =
          case dst of
            NONE   => (TextIO.stdOut, const ())
          | SOME f => (TextIO.openOut f, TextIO.closeOut)
      fun write s = TextIO.output(outs, s)
      val one = write o display 8
      fun procSingleFile f =
          let
            val (l, w, c) = wc1 f
          in List.app one [l, w, c]
           ; write $ " " ^ f ^ "\n"
          end handle Io _ => write $ f ^ " cannot be opened\n"
    in
      List.app procSingleFile files
    ; close outs
    end


(* Opgave 8a
 *)
signature MONOID =
sig
  type t
  val mul : t * t -> t
  val one : t
end

signature MONOIDWPOW =
sig
  type t
  val mul : t * t -> t
  val one : t
  val pow : t * int -> t
end

functor RepSquare (structure S : MONOID) : MONOIDWPOW =
struct
open S
fun pow (_, 0) = one
  | pow (x, 1) = x
  | pow (x, n) =
    let
      val y = pow (x, n div 2)
      val z = mul (y, y)
    in
      if n mod 2 = 0
      then z
      else mul (x, z)
    end
end

structure StringConc =
struct
type t = string
val mul = op ^
val one = ""
end
