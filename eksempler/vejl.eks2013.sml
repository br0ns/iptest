(* Vejledende løsning til eksamenssættet i
   88888888888888b.     .d8888b.  .d8888b.  d888  .d8888b.
     888  888   Y88b   d88P  Y88bd88P  Y88bd8888 d88P  Y88b
     888  888    888          888888    888  888      .d88P
     888  888   d88P        .d88P888    888  888     8888"
     888  8888888P"     .od888P" 888    888  888      "Y8b.
     888  888          d88P"     888    888  888 888    888
     888  888          888"      Y88b  d88P  888 Y88b  d88P
   8888888888          888888888  "Y8888P" 8888888"Y8888P"  *)

(* Opgave 1 *)

(* a *)
(* firstDefined giver det første element af formen SOME v i en 'a option list *)
fun firstDefined [] = NONE
  | firstDefined (SOME x :: xs) = SOME x
  | firstDefined (NONE :: xs) = firstDefined xs

(* alternativ a *)
fun firstDefined xs =
    Option.getOpt (List.find Option.isSome xs, NONE)

(* b *)
(* allDefined udpakker en 'a option list til en 'a list ved at fjerne NONE'er *)
(* allDefined svarer til biblioteksfunktionen Data.Maybe.catMaybe fra Haskell *)
fun allDefined [] = []
  | allDefined (SOME x :: xs) = x :: allDefined xs
  | allDefined (NONE :: xs) = allDefined xs

(* alternativ b *)
fun allDefined xs = List.mapPartial (fn x => x) xs


(* Opgave 2 *)

(* a *)
(* subsequence (s, t) afgør om s er en delfølge af t *)
local
  fun sublist ([], ys) = true
    | sublist (x :: xs, []) = false
    | sublist (xs0 as x :: xs, y :: ys) =
      if x = y then sublist (xs, ys) else sublist (xs0, ys)
in
  fun subsequence (s, t) = sublist (explode s, explode t)
end

(* alternativ a *)
(* Følgende alternativ undgår konvertering til char list ved at benytte
   biblioteket Substring og dens tilhørende substring-type *)
local
  open Substring
  fun subseq (NONE, NONE) = true
    | subseq (NONE, SOME _) = true
    | subseq (SOME _, NONE) = false
    | subseq (cc as (SOME (c, cs)), SOME (d, ds)) =
      if c = d then subseq (getc cs, getc ds)
               else subseq (cc, getc ds)
in
  fun subsequence (s, t) = subseq (getc (all s), getc (all t))
end


(* Opgave 3 *)

(* a *)
(* digitDivisible n afgør om alle cifrene i det positive heltal n går op i n *)
local
  fun dd (n, 0) = true
    | dd (n, m) = m mod 10 > 0
                  andalso n mod (m mod 10) = 0
                  andalso dd (n, m div 10)
in
  fun digitDivisible n = n > 0 andalso dd (n,n)
end

(* b *)
(* digitDivisibleBetween (m, n) finder cifferdivisable tal i det lukkede
   interval fra m til n *)
fun digitDivisibleBetween (m, n) =
    if m > n then [] else
    if digitDivisible m
    then m :: digitDivisibleBetween (m+1, n)
    else digitDivisibleBetween (m+1, n)

(* alternativ b *)
fun digitDivisibleBetween (m, n) =
    if m > n then []
    else List.filter digitDivisible (List.tabulate (n-m+1, fn x => x+m))

(* Opgave 4 *)

(* a *)
(* countdown n ~> [n, n-1, ..., 1, 0] *)
fun countdown n = if n < 0
                  then raise Domain
                  else List.tabulate (n + 1, fn m => n - m);

(* b *)
(* orderedList afgør om en liste er sorteret i faldende orden *)
(* Når det ikke længere gælder at a <= acc, er svaret entydigt. I stedet for at
   fortsætte rekursionen, kortsluttes foldningen med en undtagelse *)
fun orderedList [] = true
  | orderedList (x :: xs) = (foldl (fn (a, acc) =>
                                       if a <= acc
                                       then a
                                       else raise Fail "Unordered") x xs; true)
                            handle Fail "Unordered" => false;

(* alternativ b *)
;load "Listsort";
val orderedList = Listsort.sorted (fn (x, y) => Int.compare (y, x))

(* alternativ b *)
(* Eller lad det at bytte rundt på argumenterne være en højereordensfunktion: *)
local fun flip f (x, y) = f (y, x) in
  val orderedList = Listsort.sorted (flip Int.compare)
end

(* alternativ b *)
(* orderedList [4,3,2,1] ~> ListPair.all op>= ([4,3,2,1], [3,2,1])
             ~> (4 >= 3) andalso (3 >= 2) andalso (2 >= 1) ~> true *)
fun orderedList (xs as (_::ys)) = ListPair.all op>= (xs, ys)
  | orderedList _               = true

(* c *)
(* nns n giver en liste med 1 1-tal, 2 2-taller osv. op til n n-taller. *)
fun nns n =
    if n < 0 then raise Domain else
    List.concat (List.tabulate (n+1, fn i => List.tabulate (i, fn _ => i)))


(* Opgave 5 *)

type point = int * int
type drawing = point list

(* a *)
(* Gem en SVG-tegning i en fil *)
fun drawingToSVG filename drawing =
    let
      open TextIO (* openOut, output, closeOut *)
      val file = openOut filename

      (* Brug "-" som fortegn for negative koordinater *)
      fun itos n = if n < 0 then "-" ^ itos (~n) else Int.toString n

      (* Indryk <line> en smule så HTML-koden bliver pæn *)
      fun formatLine (x1,y1) (x2,y2) =
          "  <line x1=\""^itos x1^"\" y1=\""^itos y1^"\" "^
                  "x2=\""^itos x2^"\" y2=\""^itos y2^"\" />\n"

      (* output en enkelt linje *)
      fun outputLine p1 p2 = output(file, formatLine p1 p2)

      (* output en liste af punkter parvist indtil der kun er et punkt *)
      fun outputLines (p1 :: (ps as p2::_)) = (outputLine p1 p2; outputLines ps)
        | outputLines _ = ()

    in
      output(file, "<svg xmlns=\"http://www.w3.org/2000/svg\"" ^
                       " style=\"stroke: black; stroke-width: 2px;\">\n");
      outputLines drawing;
      output(file, "</svg>\n");
      closeOut file
    end


(* Opgave 6 *)

datatype command = Turn | Move of int | Repeat of int * command list
type program = command list
datatype heading = North | South | East | West
type state = point * heading

(* a *)
val draw1 = [ Move  2, Turn,
              Move  3, Turn,
              Move ~4, Turn,
              Repeat (2, [Move ~3, Turn]),
              Repeat (2, [Move 3, Turn])]

(* b *)
(* optimise optimerer et program ved at forkorte følgende mønstre:

  1) Fire Turn-kommandoer i træk
  2) To Move-kommandoer med samme fortegn i træk
  3) Repeat-kommandoer som gentages 0 gange
  4) Repeat-kommandoer som gentages præcis 1 gang (ekstra optimering)

   optimise optimerer halen først for at undgå fikspunkt-iteration. *)
fun optimise [] = []
  | optimise (cmd::cmds) =
    case cmd::optimise cmds of
        Turn::Turn::Turn::Turn::cmds'   => cmds'
      | Move 0::cmds'                   => cmds'
      | cmds'' as Move m::Move n::cmds' =>
        if m*n >= 0 then Move (m+n)::cmds' else cmds''
      | Repeat (0, _)::cmds'            => cmds'
      | Repeat (1, rep_cmds)::cmds'     => optimise (rep_cmds @ cmds')
      | Repeat (n, rep_cmds)::cmds'     => Repeat (n, optimise rep_cmds)::cmds'
      | cmds'                           => cmds'

(* c *)
(* removeRepeats fjerner alle Repeat-kommandoer fra et program uden at ændre
   programmets betydning. *)
local
  fun repeat (n, xs) = List.concat (List.tabulate (n, fn _ => xs))
in
  fun removeRepeats [] = []
    | removeRepeats (Repeat (n, rep_cmds)::cmds) =
      repeat (n, removeRepeats rep_cmds) @ removeRepeats cmds
    | removeRepeats (cmd::cmds) = cmd::removeRepeats cmds
end

(* alternativ c *)
(* Benytter højereordensfunktioner *)
local
  fun repeat n xs = List.concat (List.tabulate (n, fn _ => xs))
  fun concatMap f xs = List.concat (List.map f xs)
  fun fix (Repeat (n, cmds)) = repeat n (concatMap fix cmds)
    | fix cmd = [cmd]
in
  val removeRepeats = concatMap fix
end

(* d *)
(* eval simulerer en skildpaddes bevægelse mens den genererer en liste af de
   punkter den besøger.  eval benytter removeRepeats, hvilket gør dens plads-
   forbrug lidt større end nødvendigt. *)
local
  (* turn giver retningen 90 grader til højre *)
  fun turn North = East
    | turn East  = South
    | turn South = West
    | turn West  = North

  (* move giver punktet forskudt n skridt fra (x,y) i en givet retning *)
  fun move ((x,y), North, n) = ((x,y+n), North)
    | move ((x,y), East,  n) = ((x+n,y), East)
    | move ((x,y), South, n) = ((x,y-n), South)
    | move ((x,y), West,  n) = ((x-n,y), West)
in
  fun eval ((p,heading), [])             = [p]  (* husk slutpunktet *)
    | eval ((p,heading), Turn :: cmds)   = eval ((p, turn heading), cmds)
    | eval ((p,heading), Move n :: cmds) = p :: eval (move (p,heading,n), cmds)
    | eval (state, cmds)                 = eval (state, removeRepeats cmds)
end

(* e *)
(* Følgende løsning benytter removeRepeats og returnerer derfor nogle gange et
   længere program end input-programmet, hvor dobbelte Turns er fjernet. Denne
   følger ikke definitionen for et optimeret program fra opgave 6(b), men den
   er simplere end nedenstående alternativ og accepteres som korrekt løsning.

   Fremgangsmåden er at gemme bevægelsesretningen, dir, som starter med at være
   fremad. Hver gang to Turns kommer i træk, vendes bevægelsesretningen og disse
   to kommandoer forkastes. *)
local
  (* Bemærk at 'singleTurn' er navnet på en variabel som forventes at indeholde
     en Turn.  Selvom datatypen command egentlig også tillader Repeats her, er
     det via removeRepeats udelukket at Repeats forekommer.  I stedet for at
     lave et ekstra funktionstilfælde som aldrig bliver ramt alligevel, gribes
     begge tilfælde her. *)
  fun sto dir [] = []
    | sto dir (Turn::Turn::cmds) = sto (~dir) cmds
    | sto dir (Move n::cmds) = Move (dir * n)::sto dir cmds
    | sto dir (singleTurn::cmds) = singleTurn :: sto dir cmds
in
  fun singleTurnsOnly cmds = sto 1 (removeRepeats cmds)
  (* val singleTurnsOnly = sto 1 o removeRepeats *)
end


(* Opgave 7 *)

(* a *)
(* subListSum (n, L) finder en delliste af L hvis sum er n.

   De to basistilfælde er selvforklarende:
    - [] er en delliste hvis sum er 0
    - [] har aldrig andre summer end 0

   I det generelle tilfælde er summen forskellig fra 0 og dellisten ikke-tom.
   Da kan man spørge om det første element, n, i den ikke-tomme liste af mulige
   elementer mon indgår i summen s.

   Hvis det er tilfældet, kræver det blot at løse et mindre problem, nemlig om
   der findes en subListSum for tallet som er n mindre end s, givet en liste
   hvori n ikke indgår (da den jo er blevet brugt).

   Hvis det ikke er tilfældet, at der findes en subListSum for tallet som er n
   mindre end s, givet en liste hvori n ikke indgår, er det håbløst at forsøge
   at bruge n, og denne smides derfor væk til fordel for at søge efter en
   subListSum hvor n ikke indgår. *)
fun subListSum (0, _) = SOME []
  | subListSum (_, []) = NONE
  | subListSum (s, n :: ns) =
    case subListSum (s-n, ns) of
        NONE => subListSum (s, ns)
      | SOME n' => SOME (n :: n');

(* alternativ a *)
(* Dette alternativ er ikke på nogen måde bedre, men viser hvordan man også
   kunne have brugt en hjælpefunktion med et ekstra argument til at løse den. *)
local
  fun helper (0, _, result)     = SOME result
    | helper (s, [], _)         = NONE
    | helper (s, n::ns, result) =
      case helper (s-n, ns, n::result) of
          NONE => helper (s, ns, result)
        | SOME result => SOME result
in
  (* For at bevare rækkefølgen, er det nødvendigt at vende resultatet om.
     Da resultatet gemmer sig i en option-type, anvendes Option.map.  *)
  fun subListSum (m, ns) = Option.map rev (helper (m, ns, []))
end
