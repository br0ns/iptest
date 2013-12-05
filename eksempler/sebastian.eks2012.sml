(****************************)
(*     IP Eksamen 2012      *)
(*    Vejledende løsning    *)
(****************************)

(* Højreassociativ funktionsanvendelse.
 * f $ g $ h x  = f (g (h x))*)
infixr $
fun f $ x = f x

(* Opgave 1 {{{ *)
(* erPalindrom : string -> bool
 * Fortæller om teksten er palindromisk. *)
fun erPalindrom s =
  let val cs = explode s in
    cs = rev cs
  end;

(* erUdvidetPalindrom : string -> bool
 * Er teksten et udvidet palindrom? *)
fun erUdvidetPalindrom s =
  let
    (* s normaliseret, dvs s uden tegnsætningssymboler, blanktegn og med små i
       stedet for store bogstaver. *)
    val norm = String.translate (fn c => if Char.isSpace c orelse Char.isPunct c
                                         then ""
                                         else str (Char.toLower c)) s
  in
    erPalindrom norm
  end

(* }}} *)
(* Opgave 2 {{{ *)
datatype rute = Stop | Frem of int * rute | Drej of int * rute

(* korrekt : rute -> bool
 * Verificerer, at ruten ikke indeholder negative frem-skridt *)
fun korrekt Stop          = true
  | korrekt (Frem (d, r)) = d >= 0 andalso korrekt r
  | korrekt (Drej (_, r)) = korrekt r

(* laengde : rute -> int
 * Hvor lang er ruten? *)
fun laengde Stop          = 0
  | laengde (Frem (d, r)) = d + laengde r
  | laengde (Drej (_, r)) = laengde r

(* erNormaliseret : rute -> bool
 * Er ruten normaliseret? *)
fun erNormaliseret rt = case rt of
    Frem (0, _)           => false
  | Drej (0, _)           => false
  | Drej (_, Drej (_, _)) => false
  | Frem (_, Frem (_, _)) => false
  | Drej (g, r)           => g > ~180 andalso g <= 180 andalso erNormaliseret r
  | Frem (_, r)           => erNormaliseret r
  | Stop                  => true

local
  (* normVinkel : int -> int
   * Normaliserer en vinkel så den ligger i intervallet [-180; 179] *)
  fun normVinkel g = let val g' = g mod 360 in
                        if g' > 180 then g' - 360 else g'
                     end
in
  (* normaliserRute : rute -> rute
   * Normaliserer en rute *)
  fun normaliserRute rt = case rt of
      Stop => Stop
    | Drej (g, r) => (case Drej(g, normaliserRute r) of
            Drej(0, r')            => r'
          | Drej(g1, Drej(g2, r')) => normaliserRute $
                                          Drej (normVinkel (g1+g2), r')
          | Drej(g, r')            => Drej (normVinkel g, r')
          | r'                     => r')
    | Frem (d, r) => (case Frem(d, normaliserRute r) of
            Frem(0, r')            => r'
          | Frem(d1, Frem(d2, r')) => normaliserRute $ Frem (d1+d2, r')
          | r'                     => r')
end
(* }}} *)
(* Opgave 3 {{{ *)
(* kvadratfrit n : int -> bool
 * Er kvadratfrit; dvs, findes der et kvadrattal > 1, som går op i det? *)
fun kvadratfrit n =
  let
    val lim = floor o Math.sqrt $ real n
    fun check i = i > lim orelse n mod (i*i) <> 0 andalso check (i+1)
  in
    if n > 0 then check 2 else raise Domain
  end

(* Delopgave b:

  kvadratfrit 21 (lim = 4)
    check 2
      check 3
        check 4
          check 5
          ~> true
        ~> true
      ~> true
    ~> true
  ~> true

  kvadratfrit 54 (lim = 7)
    check 2
      check 3
      ~> false (da 54 mod (3*3) = 0)
    ~> false 
  ~> false 

*)

(* maksKvadratfrit : int -> int
 * maksKvadratfit n finder det største k, sådan at k er divisor i n, og k er
   kvadratfri *)
fun maksKvadratfrit n =
  let
    exception Impossible

    (* primDivisorer : int -> int -> int list -> int list

       primDivisorer n i ls

       Opbygger en liste af de forskellige prim-divisorer i n.
       i er det næste tal vi skal checke om går op.
       ls er en sorteret liste over hvilke tal der er gået op indtil videre,
       uden gentagelser. *)

    fun primDivisorer 1 i ls       = ls
      | primDivisorer n' i (ls as x::_) =
          if n' mod i = 0
          then primDivisorer (n' div i) i (if i = x then ls else i::ls)
          else primDivisorer n' (i+1) ls
      | primDivisorer _ _ _ = raise Impossible
  in
    if n <= 0 then raise Domain
    else if kvadratfrit n then n
    else foldl op* 1 $ primDivisorer n 2 [1]
  end;
(* }}} *)
(* Opgave 4 {{{ *)
local
  (* fjern : ''a -> ''a list -> ''a list option
     Forsøger at fjerne elementet fra listen. Hvis det ikke findes gives NONE.*)
  fun fjern x (y::ys) = if x = y then SOME ys
                                 else Option.map (fn ys' => y::ys') $ fjern x ys
    | fjern _ [] = NONE
in
  (* erPermutationAf : ''a list * ''a list -> bool
     Er de to lister permutationer af hinanden? *)
  fun erPermutationAf ([], []) = true
    | erPermutationAf ([], _)  = false
    | erPermutationAf (x::xs, ys) =
        case fjern x ys of
            NONE     => false
          | SOME ys' => erPermutationAf (xs, ys')
end

local
  (* taelDiff : ''a list -> ''a
   * Tæller antallet af forskellige elementer i listen *)
  fun taelDiff []      = []
    | taelDiff (x::xs) =
      let val (es, xs') = List.partition (fn y => x = y) xs
      in (1 + length es) :: taelDiff xs' end

  (* upto : int -> int list
   * upto n = [2, 3, ..., n] *)
  fun upto n = List.tabulate (if n-1 > 0 then n-1 else 0, fn x => x+2)

  (* gcd : int * int -> int
   * Finder største fælles divisor *)
  fun gcd (0, n) = n
    | gcd (m, n) = gcd(n mod m, m)

  (* forkort : int * int list -> int list
     Forkorter brøken (m1*m2*...*mn)/n -> (m1'*m2'*...*mk')/1
     Resultatlisten indeholder [m1', m2', ..., mk'] *)
  fun forkort (1, ms)    = ms
    | forkort (n, [])    = raise Domain
    | forkort (n, m::ms) =
      let val d = gcd (n, m) in
        if d = m
        then            forkort (n div d, ms)
        else m div d :: forkort (n div d, ms)
      end
in
  (* antalPermutationerNy : int list -> int
   * Beregner antal forskellige permutationer af listen på en effektiv måde. *)
  fun antalPermutationerNy ls =
    let val denom = List.concat $ map upto $ taelDiff ls
        val numer = upto $ length ls
    in foldl op* 1 $ foldl forkort numer denom end

  (* antalPermutationer : int list -> int
     Beregner antal forskellige permutationer af listen *)
  fun antalPermutationer ls = antalPermutationerNy ls
end
(* }}} *)
(* Opgave 5 {{{ *)
(* grupper : ('a -> ''b) -> 'a list -> ''a list list
 * grupper f ls grupperer elementerne i ls efter hvilket svar f giver på
   elementet. *)
fun grupper p []      = []
  | grupper p (x::xs) =
      let val (gs, rs) = List.partition (fn e => p x = p e) xs
      in (x::gs) :: grupper p rs end

(* gentag : ('a -> 'a) -> 'a -> 'a
 * gentag f x kører f gentagne gange på x, indtil f giver en undtagelse.
   Når f giver en undtagelse returneres værdien f fik da den gav undtagelsen. *)
fun gentag f x = gentag f $ f x handle _ => x

(* gcd : int * int -> int
 * gcd implementeret vha. gentag *)
val gcd = #2 o gentag (fn (m, n) => (n mod m, m))
(* }}} *)
(* Opgave 6 {{{ *)
datatype 'a trae = K of 'a * 'a trae list

val t7 = K(3, [
           K(4, [
             K(7, []),
             K(1, []),
             K(5, [
               K(6, []),
               K(7, [])
             ])
           ])
         ])

(* praeorden : 'a trae -> 'a list 
 * Giver elementerne i træet i præ-orden *)
fun praeorden (K(e, ts)) = e :: List.concat (map praeorden ts)

(* erstat : 'a trae * 'b list -> 'b trae * 'b list
 * Erstatter elementerne i træet i præ-orden med elementerne fra listen.
 * Resultatlisten er de resterende elementer.
 * Hvis listen er for kort kastes Domain *)
fun erstat (K(e, ts), x::xs) =
    let
      (* erstatLs : 'a trae list * 'b list -> 'b trae list * 'b list
         erstatter elementerne i et træ af gangen vha. erstat *)
      fun erstatLs ([], xs) = ([], xs)
        | erstatLs (t::ts, xs) =
            let val (t', xs')   = erstat(t, xs)
                val (ts', xs'') = erstatLs (ts, xs')
            in (t'::ts', xs'') end

      val (ts', xs') = erstatLs (ts, xs)
    in
      (K(x, ts'), xs')
    end
  | erstat (_, []) = raise Domain

;load "Listsort";
(* sorter : int trae -> int trae
 * Giver et træ med samme struktur, men sådan at elementerne taget i præorden er
   sorterede *)
fun sorter t = #1 $ erstat(t, Listsort.sort Int.compare $ praeorden t)
(* }}} *)
(* Opgave 7 {{{ *)
(* inverterPPM : string -> string -> unit
 * inverterPPM in out læser PPM-billedfilen fra stien in, inverterer farverne,
   og gemmer resultatet som en PPM i stien out. *)
fun inverterPPM infile outfile =
  let
    val is = TextIO.openIn infile
    val contents = TextIO.inputAll is
    val _ = TextIO.closeIn is

    fun rev ls = foldl op:: [] ls

    (* split : char list -> char list -> char list * char list
     * split cs [] opdeler cs i PPM hoved og krop. *)
    fun split (#"2" :: #"5" :: #"5" :: x :: xs) ls =
        (rev (x :: #"5" :: #"5" :: #"2" :: ls), xs)
      | split (x :: xs) ls = split xs (x::ls)
      | split _ _ = raise Domain

    (* invChr : char -> char
     * Inverterer et tegn *)
    fun invChr c = chr(255 - ord c)

    val (header, body) = split (explode contents) []
    val bodys = implode body

    val os = TextIO.openOut outfile

  in
    TextIO.output (os, implode header);
    TextIO.output (os, String.map invChr bodys);
    TextIO.closeOut os
  end
(* }}} *)
(* Opgave 8 {{{ *)
signature SYMBOLTABEL =
sig
  type 'vaerdi tabel
  val tom     : 'vaerdi tabel
  val indsaet : 'vaerdi tabel -> (string * 'vaerdi) -> 'vaerdi tabel
  val find    : 'vaerdi tabel -> string -> 'vaerdi option
end

(* repræsenterer en tabel som en liste af (nøgle, værdi) *)
structure ListeTabel :> SYMBOLTABEL =
struct
  type 'vaerdi tabel = (string * 'vaerdi) list
  val tom = []
  fun indsaet t (k, v) = (k, v) :: t
  fun find [] n = NONE
    | find ((k,v) :: ts) n = if k = n then SOME v else find ts n
end

(* repræsenterer en tabel som en funktion *)
structure FunTabel :> SYMBOLTABEL =
struct
  type 'vaerdi tabel = string -> 'vaerdi option
  fun tom _ = NONE
  fun indsaet t (k, v) x = if x = k then SOME v else t x
  fun find t = t
end
(* }}} *)
