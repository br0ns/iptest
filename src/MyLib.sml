signature GENERAL =
sig
  eqtype unit
  type exn = exn

  exception Bind
  exception Match
  exception Chr
  exception Div
  exception Domain
  exception Fail of string
  exception Overflow
  exception Size
  (* exception Span *)
  exception Subscript

  val exnName : exn -> string
  val exnMessage : exn -> string

  datatype order = LESS | EQUAL | GREATER
  val ! : 'a ref -> 'a
  val := : 'a ref * 'a -> unit
  (* val o : ('b -> 'c) * ('a -> 'b) -> 'a -> 'c *)
  (* val before : 'a * unit -> 'a *)
  val ignore : 'a -> unit
end
signature General =
sig
  (* infix 5 ^* to
   * infix 4 \< \>
   * infix 3 $
   *)
  include GENERAL where type unit = unit
                    and type order = order

  val id : 'a -> 'a
  val ^* : ('a -> 'a) * int -> 'a -> 'a
  val $ : ('a -> 'b) * 'a -> 'b
  val flip : ('a * 'b -> 'c) -> 'b * 'a -> 'c
  val \< : 'a * ('a * 'b -> 'c) -> 'b -> 'c
  val \> : 'b * ('a * 'b -> 'c) -> 'a -> 'c

  val flipc : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
  val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  val curry3 : ('a * 'b * 'c -> 'd) -> 'a -> 'b -> 'c -> 'd
  val uncurry3 : ('a -> 'b -> 'c -> 'd) -> 'a * 'b * 'c -> 'd
  val curry4 : ('a * 'b * 'c * 'd -> 'e) ->
               'a -> 'b -> 'c -> 'd -> 'e
  val uncurry4 : ('a -> 'b -> 'c -> 'd -> 'e) ->
                 'a * 'b * 'c * 'd -> 'e
  val pair : 'a -> 'b -> 'a * 'b
  val triple : 'a -> 'b -> 'c -> 'a * 'b * 'c
  val quadruple : 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
  val to : int * int -> int list
  val inc : int ref -> int
  val dec : int ref -> int
  val const : 'a -> 'b -> 'a
end
signature TEXT_IO = TextIO
signature STRING =
sig
  eqtype string
  eqtype char
  val maxSize : int
  val size : string -> int
  val sub : string * int -> char
  val extract   : string * int * int option -> string
  val substring : string * int * int -> string
  val ^ : string * string -> string
  val concat : string list -> string
  val concatWith : string -> string list -> string
  val str : char -> string
  val implode : char list -> string
  val explode : string -> char list
  val map : (char -> char) -> string -> string
  val translate : (char -> string) -> string -> string
  val tokens : (char -> bool) -> string -> string list
  val fields : (char -> bool) -> string -> string list
  val isPrefix    : string -> string -> bool
  val isSubstring : string -> string -> bool
  val isSuffix    : string -> string -> bool
  val compare : string * string -> order
  val collate : (char * char -> order)
                -> string * string -> order
  val <  : string * string -> bool
  val <= : string * string -> bool
  val >  : string * string -> bool
  val >= : string * string -> bool

  val toString : string -> String.string
  val scan       : (char, 'a) StringCvt.reader
                   -> (string, 'a) StringCvt.reader
  val fromString : String.string -> string option
  val toCString : string -> String.string
  val fromCString : String.string -> string option
end
structure Timer =
struct
open Timer
fun checkCPUTimer timer =
    case Timer.checkCPUTimer timer of
      {usr, sys, ...} => {usr = usr, sys = sys}
end
structure String =
struct
open String
type char = char
fun concatWith s xs =
    case xs of
      nil     => ""
    | [x]     => x
    | x :: xs => x ^ s ^ concatWith s xs
fun scan getc strm = SOME ("", strm)
end
structure Substring =
struct
open Substring
fun concatWith s = String.concatWith s o map string
val full = all
end
structure General :> General =
struct
open General
fun id x = x
fun const a b = a
fun flipc f a b = f b a
fun flip f (a, b) = f (b, a)
fun \< (a, f) b = f (a, b)
fun \> (b, f) a = f (a, b)

fun $ (f, v) = f v

fun ^* (_, 0) = id
  | ^* (f, n) = f o (^* (f, n - 1))

fun curry f a b = f (a, b)
fun uncurry f (a, b) = f a b
fun pair a b = (a, b)

fun curry3 f a b c = f (a, b, c)
fun uncurry3 f (a, b, c) = f a b c
fun triple a b c = (a, b, c)

fun curry4 f a b c d = f (a, b, c, d)
fun uncurry4 f (a, b, c, d) = f a b c d
fun quadruple a b c d = (a, b, c, d)

fun to (a, b) =
    if a <= b then
      a :: to (a + 1, b)
    else
      nil

fun inc x = (x := !x + 1 ; !x)
fun dec x = (x := !x - 1 ; !x)
end
signature TextIO =
sig
  include TEXT_IO 
  where type instream = TextIO.instream
  and type outstream  = TextIO.outstream

  val println : string -> unit
  val readFile : string -> string
  val writeFile : string -> string -> unit
  val appendFile : string -> string -> unit
end
structure TextIO :> TextIO =
struct
open TextIO

fun println s = (print s ; print "\n")

fun readFile f =
    let
      val is = openIn f
    in
      inputAll is before closeIn is
    end

fun writeFile f s =
    let
      val os = openOut f
    in
      output (os, s) before closeOut os
    end

fun appendFile f s =
    let
      val os = openAppend f
    in
      output (os, s) before closeOut os
    end
end
signature String =
sig
  include STRING
  where type string = string
    and type char = Char.char

val tabulate : int * (int -> char) -> string

(* Max width -> text -> wordwrapped text *)
val wordwrap : int -> string -> string

(* Tab width -> text with tabs -> text without tabs *)
val untabify : int -> string -> string

val spaces : int -> string

val <- : string * string -> string
end
structure String :> String =
struct
open String
val tabulate = CharVector.tabulate

fun spaces n = tabulate (n, fn _ => #" ")

fun wordwrap width text =
    let
      fun line (n, nil) = (nil, nil)
        | line (n, w :: ws) =
          if Int.<= (n + 1 + Substring.size w, width) then
            let
              val (ws, ws') = line (n + 1 + Substring.size w, ws)
            in
              (w :: ws, ws')
            end
          else
            (nil, w :: ws)
      fun lines nil = nil
        | lines ws =
          let
            val (ln, ws) = line (0, ws)
          in
            ln :: lines ws
          end
    in
      (String.concatWith "\n" o
       List.map (Substring.concatWith " ") o
       List.concat o
       List.map lines o
       List.map (Substring.fields (General.curry op= #" ")) o
       Substring.fields (General.curry op= #"\n") o
       Substring.full) text
    end

fun untabify tabw = translate (fn #"\t" => spaces tabw | s => str s)

fun <- (s, x) =
    let
      open Substring
      val ss = full s
      fun loop i =
          if sub (ss, i) = #"%" then
            i
          else
            loop (i + 1)
      val i = loop 0
    in
      concat [slice (ss, 0, SOME i), full x, slice (ss, i + 1, NONE)]
    end handle Subscript => s
end
(* Memoized lazy evaluation *)

signature Lazy =
sig
  type 'a t
  type 'a thunk = unit -> 'a

  val lazy : 'a thunk -> 'a t
  val force : 'a t -> 'a
  val eager : 'a -> 'a t
  val delay : 'a t thunk -> 'a t
end
structure Lazy :> Lazy =
struct
type 'a thunk = unit -> 'a
datatype 'a t' = Thunk     of 'a thunk
               | Value     of 'a
               | Exception of exn
type 'a t = 'a t' ref

fun lazy f = ref (Thunk f)
fun eager v = ref (Value v)
fun force t =
    case !t of
      Thunk f =>
      (let
         val v = f ()
       in
         t := Value v ;
         v
       end
       handle e =>
              (t := Exception e;
               raise e)
      )
    | Value v => v
    | Exception e => raise e
fun delay t = lazy (fn _ => force (t ()))
end
signature Either =
sig
  datatype ('a, 'b) t = Left of 'a | Right of 'b
  exception Either

  val ofLeft : ('a, 'b) t -> 'a
  val ofRight : ('a, 'b) t -> 'b
  val either : ('a -> 'c) -> ('b -> 'c) -> ('a, 'b) t -> 'c
  val lefts : ('a, 'b) t list -> 'a list
  val rights : ('a, 'b) t list -> 'b list
  val partition : ('a, 'b) t list -> 'a list * 'b list
end
structure Either :> Either =
struct
datatype ('a, 'b) t = Left of 'a | Right of 'b
exception Either

fun ofLeft (Left x) = x
  | ofLeft _ = raise Either

fun ofRight (Right x) = x
  | ofRight _ = raise Either

fun either l r e =
    case e of
      Left x  => l x
    | Right x => r x

fun lefts es = List.mapPartial (fn Left x => SOME x | _ => NONE) es
fun rights es = List.mapPartial (fn Right x => SOME x | _ => NONE) es

fun partition es = (lefts es, rights es)
end
signature Arrow =
sig
  (* infix 4 ** && ++ || \< \>
   * infix 3 << >>
   *)
  type ('a, 'b) t = 'a -> 'b

  val id : ('a, 'a) t
  val const : 'a -> ('b, 'a) t
  val flip : ('a * 'b, 'c) t -> ('b * 'a, 'c) t

  val first : ('a, 'b) t -> ('a * 'c, 'b * 'c) t
  val second : ('a, 'b) t -> ('c * 'a, 'c * 'b) t

  val ** : ('a, 'b) t * ('c, 'd) t -> ('a * 'c, 'b * 'd) t
  val && : ('a, 'b) t * ('a, 'c) t -> ('a, 'b * 'c) t

  val left : ('a, 'b) t -> (('a, 'c) Either.t, ('b, 'c) Either.t) t
  val right : ('a, 'b) t -> (('c, 'a) Either.t, ('c, 'b) Either.t) t

  val ++ : ('a, 'b) t * ('c, 'd) t -> (('a, 'c) Either.t, ('b, 'd) Either.t) t
  val || : ('a, 'c) t * ('b, 'c) t -> (('a, 'b) Either.t, 'c) t

  val >> : ('a, 'b) t * ('b, 'c) t -> ('a, 'c) t
  val << : ('b, 'c) t * ('a, 'b) t -> ('a, 'c) t

  val \< : 'a * ('a * 'b, 'c) t -> ('b, 'c) t
  val \> : 'b * ('a * 'b, 'c) t -> ('a, 'c) t
end
structure Arrow :> Arrow =
struct
infix 4 ** && ++ ||
infix 3 >> <<
type ('a, 'b) t = 'a -> 'b

val const = General.const
val \< = General.\<
val \> = General.\>
val flip = General.flip
val id = General.id

fun first a (x, y) = (a x, y)
fun second a (x, y) = (x, a y)

fun (a ** b) (x, y) = (a x, b y)

fun (a && b) x = (a x, b x)

fun (a ++ b) z =
    case z of
      Either.Left x  => Either.Left (a x)
    | Either.Right y => Either.Right (b y)

fun left a = a ++ id
fun right a = id ++ a

fun (a || b) z =
    case (a ++ b) z of
      Either.Left x  => x
    | Either.Right y => y

fun (a >> b) = b o a
fun (a << b) = a o b
end
signature LazyList =
sig
  datatype 'a t' = Cons of 'a * 'a t' Lazy.t
                 | Nil
  type 'a t = 'a t' Lazy.t

  val eager : 'a List.list -> 'a t
  val force : 'a t -> 'a List.list
  (* Alias for getItem *)
  val split : 'a t -> ('a * 'a t) option
  val cons : 'a Lazy.t * 'a t -> 'a t

  (* val nil : 'a t
   * Because of value polymorphism nil can't be defined. Insted use
   * eager nil
   *)

  val consEager : 'a * 'a t -> 'a t
  val singleton : 'a -> 'a t

  (* These work just like for regular lists. So see
   * http://www.standardml.org/Basis/list.html
   *
   * Note: Since lazy list have the potential to be infinit these functions
   * can be hazardous: length, last, app, find, foldl, foldr, exists, all
   * (raising an exception can stop the evaluation of a (possibly infinit)
   * list).
   *
   * Note': The functions are generally as lazy as possible. Example: while
   * {take ([1], 2)} raises Subscript for regular lists {take (eager [1], 2)}
   * only raises Subscript if more than one element of the resulting lazy list
   * is ever consumed. *)
  val null : 'a t -> bool
  val length : 'a t -> int
  val @ : 'a t * 'a t -> 'a t
  val hd : 'a t -> 'a
  val tl : 'a t -> 'a t
  val last : 'a t -> 'a
  val getItem : 'a t -> ('a * 'a t) option
  val nth : 'a t * int -> 'a
  val take : 'a t * int -> 'a t
  val drop : 'a t * int -> 'a t
  val rev : 'a t -> 'a t
  val concat : 'a t t -> 'a t
  val revAppend : 'a t * 'a t -> 'a t
  val app : ('a -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val mapPartial : ('a -> 'b option) -> 'a t -> 'b t
  val find : ('a -> bool) -> 'a t -> 'a option
  val filter : ('a -> bool) -> 'a t -> 'a t
  val partition : ('a -> bool) -> 'a t -> 'a t * 'a t
  val foldl : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val foldr : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
  val exists : ('a -> bool) -> 'a t -> bool
  val all : ('a -> bool) -> 'a t -> bool
  val tabulate : int * (int -> 'a) -> 'a t
  val collate : ('a * 'a -> order) -> 'a t * 'a t -> order

  (* Extra *)
  (* In increasing magnitude, positive numbers before negative. The naturals start at 0 *)
  exception Stop
  val tabulateN : (int -> 'a) -> 'a t
  (* val tabulateZ : (int -> 'a) -> 'a t *)
  (* val tabulateQ : (int * int -> 'a) -> 'a t *)
  (* val tabulatePrimes : (int -> 'a) -> 'a t *)
  (* val tabulateFibonacci : (int -> 'a) -> 'a t *)
  (* val tabulateBell : (int -> 'a) -> 'a t *)
  (* val tabulateCatalan : (int -> 'a) -> 'a t *)
  (* val tabulateFactorial : (int -> 'a) -> 'a t *)

  (* val sort : ('a -> 'a -> order) -> 'a t -> 'a t *)
  (* val shuffle : 'a t -> 'a t *)
  (* val leftmost  : 'a option t -> 'a option *)
  (* val rightmost : 'a option t -> 'a option *)
  val allPairs : 'a t -> 'b t -> ('a * 'b) t
  (* val splitAt : 'a t * int -> 'a t * 'a t *)
  (* val allSplits : 'a t -> ('a t * 'a t) t *)
  (* val consAll : 'a * 'a t t -> 'a t t *)
  (* val concatMap : ('a -> 'b t) -> 'a t -> 'b t *)
  (* val range : int -> int -> 'a t -> 'a t *)
  (* val power : 'a t -> 'a t t *)
  (* val group : ('a -> 'a -> bool) -> 'a t -> 'a t t *)

  val fromFile : string -> char t
end
structure LazyList :> LazyList =
struct
open General Lazy infix 2 $ infix 4 \< \> infix 5 ^* to
datatype 'a t' = Cons of 'a * 'a t
               | Nil
withtype 'a t = 'a t' Lazy.t

val F = force
val E = eager
val L = lazy

fun eager nil = E Nil
  | eager (x :: xs) = E $ Cons (x, eager xs)

fun force xs =
    case F xs of
      Cons (x, xs) => x :: force xs
    | Nil          => nil

fun split l =
    case F l of
      Cons x => SOME x
    | Nil    => NONE

fun consEager x = E $ Cons x

fun cons (x, xs) = L (fn _ => Cons (F x, xs))

fun singleton x = eager [x]

fun null l =
    case F l of
      Nil => true
    | _   => false

infixr 5 @
fun xs @ ys =
    L (fn _ =>
          case F xs of
            Cons (x, xs) => Cons (x, xs @ ys)
          | Nil          => F ys
      )

fun hd l =
    case F l of
      Cons (x, _) => x
    | Nil         => raise Empty

fun tl l =
    case F l of
      Cons (_, xs) => xs
    | Nil          => raise Empty

fun last l =
    let
      fun loop (x, l) =
          case F l of
            Cons x => loop x
          | Nil    => x
    in
      case F l of
        Cons x => loop x
      | Nil    => raise Empty
    end

val getItem = split

fun nth (l, n) = hd o tl ^* n $ l handle Empty => raise Subscript

fun take (l, n) =
    L (fn _ =>
          if n = 0 then
            Nil
          else
            case F l of
              Cons (x, xs) => Cons (x, take (xs, n - 1))
            | Nil          => raise Subscript
      )

fun drop (l, n) = tl ^* n $ l

fun rev l = (eager o List.rev o force) l

fun concat ls =
    L (fn _ =>
          case F ls of
            Cons (l, ls) => F $ l @ concat ls
          | Nil          => Nil
      )

fun revAppend _ = raise Fail "Lazy.List.revAppend unimplemented"

fun app f = List.app f o force

fun map f l =
    L (fn _ =>
          case F l of
            Cons (x, xs) => Cons (f x, map f xs)
          | Nil          => Nil
      )

fun mapPartial f l =
    L (fn _ =>
          case F l of
            Cons (x, xs) =>
            (case f x of
               SOME x => Cons (x, mapPartial f xs)
             | NONE   => F $ mapPartial f xs
            )
          | Nil          => Nil
      )

fun find p l =
    case F l of
      Cons (x, xs) =>
      if p x then
        SOME x
      else
        find p xs
    | Nil          => NONE

fun filter p l =
    L (fn _ =>
          case F l of
            Cons (x, xs) =>
            if p x then
              Cons (x, filter p xs)
            else
              F $ filter p xs
          | Nil          => Nil
      )

fun partition p l = (filter p l, filter (not o p) l)

fun foldl f b l =
    case F l of
      Cons (x, xs) => foldl f (f (x, b)) xs
    | Nil          => b

fun length l = foldl (fn (_, a) => 1 + a) 0 l

fun foldr f b = List.foldr f b o List.rev o force

fun exists p l =
    case F l of
      Cons (x, xs) => p x orelse exists p xs
    | Nil          => false

fun all p l =
    case F l of
      Cons (x, xs) => p x andalso all p xs
    | Nil          => true

fun tabulate (n, f) =
    let
      fun loop i =
          if i = n then
            E Nil
          else
            L (fn _ => Cons (f i, loop (i + 1)))
    in
      loop 0
    end

exception Stop
fun tabulateN f =
    let
      fun loop n =
          L (fn _ => Cons (f n, loop (n + 1))
                handle Stop => Nil
            )
    in
      loop 0
    end

fun collate cmp (xs, ys) =
    case (F xs, F ys) of
      (Cons (x, xs), Cons (y, ys)) =>
      (case cmp (x, y) of
         EQUAL => collate cmp (xs, ys)
       | x     => x
      )
    | (Cons _, Nil)                => GREATER
    | (Nil, Cons _)                => LESS
    | (Nil, Nil)                   => EQUAL

fun allPairs xs ys =
    E Nil

fun fromFile f =
    let
      val is = TextIO.openIn f
      fun loop _ =
          case TextIO.input1 is of
            SOME c => Cons (c, L loop)
          | NONE   => Nil before TextIO.closeIn is
    in
      L loop
    end

end
signature Benchmark =
sig
    val start : unit -> unit
    val pause : unit -> unit
    val stop : unit -> unit
    val time : unit -> {usr : Time.time, sys : Time.time, tot : Time.time}

    (* Equivalent to stop() ; start () *)
    val restart : unit -> unit

    val show : unit -> string

    val print : string -> unit
    val stopAndPrint : string -> unit
end
structure Benchmark :> Benchmark =
struct
val zeroTime = {usr = Time.zeroTime, sys = Time.zeroTime}
val dummyTimer = Timer.totalCPUTimer ()

val time = ref zeroTime
val time' = ref zeroTime
val timer = ref dummyTimer

fun ++ {usr = u1, sys = s1} =
    let
      val {usr = u2, sys = s2} = !time
    in
      time := {usr = Time.+ (u1, u2), sys = Time.+ (s1, s2)}
    end

fun start () = timer := Timer.startCPUTimer ()
fun pause () = (++ (Timer.checkCPUTimer (!timer)) ; time' := !time)
fun stop () = (pause () ; time := zeroTime)
fun restart () = (stop () ; start ())
fun time () =
    let
      val {usr, sys} = !time'
    in
      {usr = usr, sys = sys, tot = Time.+ (usr, sys)}
    end
fun show () =
    let
      val {usr, sys, tot} = time ()
    in
      "User: " ^ Time.toString usr ^ ", " ^
      "System: " ^ Time.toString sys ^ ", " ^
      "Total: " ^ Time.toString tot
    end

fun print "" = TextIO.print (show () ^ "\n")
  | print s = TextIO.print (s ^ "\n  " ^ show () ^ "\n")
fun stopAndPrint s = (stop () ; print s)
end
signature Show =
sig
    val int : int -> string
    val real : real -> string
    val bool : bool -> string
    val char : char -> string
    val string : string -> string
    val option : ('a -> string) -> 'a option -> string
    val order : order -> string

    val pair : ('a -> string) ->
               ('b -> string) ->
               'a * 'b -> string
    val triple : ('a -> string) ->
                 ('b -> string) ->
                 ('c -> string) ->
                 'a * 'b * 'c -> string
    val quadruple : ('a -> string) ->
                    ('b -> string) ->
                    ('c -> string) ->
                    ('d -> string) ->
                    'a * 'b * 'c * 'd -> string

    val list : ('a -> string) -> 'a list -> string
end
structure Show =
struct
    val int = Int.toString
    val real = Real.toString
    val bool = Bool.toString
    fun char c = "#\"" ^ str c ^ "\""
    fun string s = "\"" ^ s ^ "\""

    fun option _ NONE = "NONE"
      | option show (SOME x) = "SOME(" ^ show x ^ ")"

    fun order LESS = "LESS"
      | order EQUAL = "EQUAL"
      | order GREATER = "GREATER"

    fun pair showa showb (a, b) = "(" ^ showa a ^ ", " ^ showb b ^ ")"
    fun triple showa showb showc (a, b, c)  = "(" ^ showa a ^ ", " ^ showb b ^ ", " ^ showc c ^ ")"

    fun quadruple showa showb showc showd (a, b, c, d) =
        "(" ^ showa a ^ ", " ^ showb b ^ ", " ^
        showc c ^ ", " ^ showd d ^ ")"

    fun list show xr =
        let
            fun list' nil = ""
              | list' [x] = show x
              | list' (x :: xr) = show x ^ ", " ^ list' xr
        in
            "[" ^ list' xr ^ "]"
        end
end
signature Pair =
sig
  type ('a, 'b) t = 'a * 'b

  (* structure Iso : *)
  (*           sig *)
  (*             val toPair *)
  (*             val fromPair *)
  (*             val toTriple *)
  (*             val fromTriple *)
  (*             val toQuadruple *)
  (*             val fromQuadruple *)
  (*           end *)

  val swap : 'a * 'b -> 'b * 'a
  val swizzle : ('a * 'b) * ('c * 'd) -> ('a * 'c) * ('b * 'd)
  val fst : 'a * 'b -> 'a
  val snd : 'a * 'b -> 'b
  val app : ('a -> unit) * ('b -> unit) -> 'a * 'b -> unit
  val appFst : ('a -> unit) -> 'a * 'b -> unit
  val appSnd : ('b -> unit) -> 'a * 'b -> unit
  val map : ('a -> 'c) * ('b -> 'd) -> 'a * 'b -> 'c * 'd
  val mapFst : ('a -> 'c) -> 'a * 'b -> 'c * 'b
  val mapSnd : ('b -> 'c) -> 'a * 'b -> 'a * 'c
  val foldl : ('a * 'c -> 'c) * ('b * 'c -> 'c) -> 'c -> 'a * 'b -> 'c
  val foldr : ('a * 'c -> 'c) * ('b * 'c -> 'c) -> 'c -> 'a * 'b -> 'c
  val delay : 'a Lazy.t * 'b Lazy.t -> ('a * 'b) Lazy.t
end
structure Pair :> Pair =
struct
type ('a, 'b) t = 'a * 'b

fun swap (a, b) = (b, a)
fun swizzle ((a, b), (c, d)) = ((a, c), (b, d))
fun fst (a, _) = a
fun snd (_, b) = b
fun app (fa, fb) (a, b) = (fa a ; fb b)
fun appFst f = f o fst
fun appSnd f = f o snd
fun map (fa, fb) (a, b) = (fa a, fb b)
fun mapFst f = map (f, General.id)
fun mapSnd f = map (General.id, f)
fun foldl (fa, fb) c (a, b) = fb (b, fa (a, c))
fun foldr (fa, fb) c (a, b) = fa (a, fb (b, c))
fun delay (ta, tb) = Lazy.lazy (fn _ => (Lazy.force ta, Lazy.force tb))
end
(* TODO: Uncurry compare functions everywhere. That makes it easier to use built
 * in and anonymous compare functions *)
signature Ordered =
sig
    eqtype t
    val compare  : t -> t -> order
end
(* Finite set data structure *)

signature Set =
sig
    type ''a t

    val empty      : ''a t
    val singleton  : ''a -> ''a t
    val insert     : ''a t -> ''a -> ''a t
    val delete     : ''a t -> ''a -> ''a t
    val fromList   : ''a list -> ''a t

    val union      : ''a t -> ''a t -> ''a t
    (* val concat     : ''a t list -> ''a t *)
    val inter      : ''a t -> ''a t -> ''a t
    val diff       : ''a t -> ''a t -> ''a t

    (* subset s s': all elements in s' is also in s *)
    val subset     : ''a t -> ''a t -> bool
    val equal      : ''a t -> ''a t -> bool
    val member     : ''a t -> ''a -> bool
    val isEmpty    : ''a t -> bool

    val toList     : ''a t -> ''a list

    val card       : ''a t -> int

    val collate    : (''a -> ''a -> order) -> ''a t -> ''a t -> order
    (* val power      : ''a t -> ''a t t *)

    val partition  : (''a -> bool) -> ''a t -> ''a t * ''a t
    val filter     : (''a -> bool) -> ''a t -> ''a t
    val exists     : (''a -> bool) -> ''a t -> bool
    val all        : (''a -> bool) -> ''a t -> bool
    val find       : (''a -> bool) -> ''a t -> ''a option

    val app        : (''a -> unit) -> ''a t -> unit
    val map        : (''a -> ''b) -> ''a t -> ''b t
    val mapPartial : (''a -> ''b option) -> ''a t -> ''b t
    val fold       : (''a * 'b -> 'b) -> 'b -> ''a t -> 'b

    val split      : ''a t -> ''a * ''a t

    (* May raise Empty *)
    val some       : ''a t -> ''a
end


structure ListSet :> Set =
struct

    type ''a t = ''a list

    val empty = nil

    fun singleton a = [a]

    fun member ys x = List.exists (fn y => x = y) ys

    fun insert ys x = if member ys x then ys else x :: ys

    fun delete nil _ = nil
      | delete (y :: ys) x = if y = x then ys else y :: delete ys x

    fun insertList ys xs = foldl (fn (x, ys) => insert ys x) ys xs

    fun fromList xs = insertList empty xs

    fun union xs ys = insertList xs ys

    fun inter xs ys = List.filter (fn x => member ys x) xs

    fun diff xs ys = List.filter (fn x => not (member ys x)) xs

    fun subset xs ys = List.all (fn x => member ys x) xs

    fun equal xs ys = subset xs ys andalso subset ys xs

    val isEmpty = null

    fun toList xs = xs

    val card = List.length

    fun collate cmp = raise Fail "Not implementet"

    val partition = List.partition

    val filter = List.filter

    fun remove p s = #2 (partition p s)

    val exists = List.exists

    val all = List.all

    val find = List.find

    val app = List.app

    fun map f s = foldl (fn (y, ys) => insert ys (f y)) empty s

    fun mapPartial f s =
        foldl (fn (y, ys) =>
                  case f y of
                    SOME y => insert ys y
                  | NONE   => ys
              )
              empty s

    val fold = foldl

    fun split nil = raise Empty
      | split (x::xs) = (x, xs)

    fun some (x :: _) = x
      | some _ = raise Empty

    fun toString _ nil = "{}"
      | toString pr (h :: t) =
        "{" ^
        foldl (fn (x, a) => a ^ ", " ^ pr x) (pr h) t ^
        "}"
end
(* Default Set implementation *)

structure Set = ListSet
(* Finite map data structure with ordered keys *)

signature OrderedMap =
sig
    type key
    type 'a t

    val empty       : 'a t

    val singleton   : key * 'a -> 'a t

    (* Insert if key is not in map *)
    val insert      : 'a t -> key * 'a -> 'a t option
    val fromList    : (key * 'a) list -> 'a t

    (* Inserts or overwrites *)
    val update      : 'a t -> key * 'a -> 'a t

    (* Does nothing if the key is not in the maps domain *)
    val delete      : 'a t -> key -> 'a t

    (* May raise Domain *)
    val remove      : 'a t -> key -> 'a * 'a t
    val modify      : ('a -> 'a) -> 'a t -> key -> 'a t
    val lookup      : 'a t -> key -> 'a option

    val inDomain    : 'a t -> key -> bool
    val isEmpty     : 'a t -> bool

    val size        : 'a t -> int

    (* Ordered by keys - least to greatest *)
    val toList      : 'a t -> (key * 'a) list
    val domain      : 'a t -> key list
    val range       : 'a t -> 'a list

    (* May raise Empty *)
    val first       : 'a t -> 'a
    val firsti      : 'a t -> key * 'a
    val last        : 'a t -> 'a
    val lasti       : 'a t -> key * 'a

    val split       : 'a t -> (key * 'a) * 'a t
    val splitFirst  : 'a t -> (key * 'a) * 'a t
    val splitLast   : 'a t -> (key * 'a) * 'a t

    val collate     : ('a -> 'a -> order) -> 'a t -> 'a t -> order

    val partition   : ('a -> bool) -> 'a t -> 'a t * 'a t
    val partitioni  : (key * 'a -> bool) -> 'a t -> 'a t * 'a t
    val filter      : ('a -> bool) -> 'a t -> 'a t
    val filteri     : (key * 'a -> bool) -> 'a t -> 'a t
    val exists      : ('a -> bool) -> 'a t -> bool
    val existsi     : (key * 'a -> bool) -> 'a t -> bool
    val all         : ('a -> bool) -> 'a t -> bool
    val alli        : (key * 'a -> bool) -> 'a t -> bool
    val find        : ('a -> bool) -> 'a t -> key * 'a
    val findi       : (key * 'a -> bool) -> 'a t -> key * 'a

    (* These go from least to greatest *)
    val app         : ('a -> unit) -> 'a t -> unit
    val appi        : (key * 'a -> unit) -> 'a t -> unit
    val map         : ('a -> 'b) -> 'a t -> 'b t
    val mapi        : (key * 'a -> 'b) -> 'a t -> 'b t
    val mapPartial  : ('a -> 'b option) -> 'a t -> 'b t
    val mapPartiali : (key * 'a -> 'b option) -> 'a t -> 'b t
    val foldl       : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldli      : ((key * 'a) * 'b) -> 'b -> 'a t -> 'b

    val foldr       : ('a * 'b -> 'b) -> 'b -> 'a t -> 'b
    val foldri      : ((key * 'a) * 'b) -> 'b -> 'a t -> 'b

    val union       : ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    val unioni      : (key -> 'a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
    (* return a map whose domain is the union of the domains of the two input
     * maps, using the supplied function to define the map on elements that
     * are in both domains.
     *)

    (* return a map whose domain is the intersection of the domains of the
     * two input maps, using the supplied function to define the range.
     *)
    val inter       : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val interi      : (key -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t

    (* merge two maps using the given function to control the merge. For
     * each key k in the union of the two maps domains, the function
     * is applied to the image of the key under the map.  If the function
     * returns SOME y, then (k, y) is added to the resulting map.
     *)
    val merge       : ('a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t
    val mergi       : (key -> 'a option -> 'b option -> 'c option) -> 'a t -> 'b t -> 'c t

    (* Return a map whose domain is the union of the domains of the two input
     * maps, always choosing the second map on elements that are in bot domains.
     *)
    val plus        : 'a t -> 'a t -> 'a t
end


functor ListOrderedMapFn (Key : Ordered) :> OrderedMap where type key = Key.t =
struct

    type key = Key.t
    type 'a t = (key * 'a) list

    val compare = Key.compare

    val empty = nil

    fun singleton x = [x]

    fun insert nil x = SOME [x]
      | insert ((k', v') :: ys) (k, v) =
        case compare k k' of
            GREATER =>
            (case insert ys (k, v) of
                 SOME ys => SOME ((k', v') :: ys)
               | NONE    => NONE
            )
          | EQUAL   => NONE
          | LESS    => SOME ((k, v) :: (k', v') :: ys)

    fun remove nil _ = raise Domain
      | remove ((k', v') :: ys) k =
        case compare k k' of
            GREATER =>
            let
                val (v, ys') = remove ys k
            in
                (v, (k', v') :: ys')
            end
          | EQUAL   => (v', ys)
          | LESS    => raise Domain

    fun delete nil _ = nil
      | delete ((k', v') :: ys) k =
        case compare k k' of
            GREATER => (k', v') :: delete ys k
          | _       => ys

    fun update nil x = [x]
      | update ((k', v') :: ys) (k, v) =
        case compare k k' of
            GREATER => (k', v') :: update ys (k, v)
          | EQUAL   => (k, v) :: ys
          | LESS    => (k, v) :: (k', v') :: ys

    fun fromList xs = foldl (fn (x, m) => update m x) empty xs

    fun modify _ nil _ = raise Domain
      | modify f ((k', v') :: ys) k =
        case compare k k' of
            GREATER => (k', v') :: modify f ys k
          | EQUAL   => (k', f v') :: ys
          | LESS    => raise Domain

    fun lookup nil _ = NONE
      | lookup ((k', v') :: ys) k =
        case compare k k' of
            GREATER => lookup ys k
          | EQUAL   => SOME v'
          | LESS    => NONE

    fun inDomain nil _ = false
      | inDomain ((k', _) :: ys) k =
        case compare k k' of
            GREATER => inDomain ys k
          | EQUAL   => true
          | LESS    => false

    val isEmpty = null

    val size = List.length

    fun toList ys = ys

    fun domain ys = map (fn (k, _) => k) ys

    fun range ys = map (fn (_, v) => v) ys

    fun first nil = raise Empty
      | first ((_, v) :: _) = v

    fun firsti nil = raise Empty
      | firsti (y :: _) = y

    fun last nil = raise Empty
      | last ys = ((fn (_, v) => v) o hd o rev) ys

    fun lasti nil = raise Empty
      | lasti ys = hd (rev ys)

    fun splitFirst nil = raise Empty
      | splitFirst (y :: ys) = (y, ys)

    fun splitLast xs = splitFirst (rev xs)

    val split = splitFirst

    fun unimp _ = raise Fail "Not implemented"

    val collate = unimp

    val partition = unimp
    val partitioni = unimp
    val filter = unimp
    val filteri = unimp
    val exists = unimp
    val existsi = unimp
    val all = unimp
    val alli = unimp
    val find = unimp
    val findi = unimp

    val app = unimp
    val appi = unimp
    val map = unimp
    val mapi = unimp
    val mapPartial = unimp
    val mapPartiali = unimp
    val foldl = unimp
    val foldli = unimp
    val foldr = unimp
    val foldri = unimp

    val union = unimp
    val unioni = unimp
    val inter = unimp
    val interi = unimp

    val merge = unimp
    val mergi = unimp

    val plus = unimp

    val toString = unimp
end
functor UnbalancedOrderedMapFn (Key : Ordered) :> OrderedMap where type key = Key.t =
struct
fun die s = raise Fail ("UnbalancedOrderedMapFn: " ^ s ^ " is unimplemented")

type key = Key.t

datatype 'a t = E
              | T of 'a t * (key * 'a) * 'a t

val empty = E

fun singleton x = T (E, x, E)

fun insert E x = SOME (singleton x)
  | insert (T (l, y as (k', _), r)) (x as (k, _)) =
    case Key.compare k k' of
      GREATER =>
      (case insert r x of
         SOME r' => SOME (T (l, y, r'))
       | NONE => NONE)
    | LESS    =>
      (case insert l x of
         SOME l' => SOME (T (l', y, r))
       | NONE => NONE)
    | EQUAL   => NONE

fun update E x = singleton x
  | update (t as T (l, y as (k', _), r)) (x as (k, _)) =
    case Key.compare k k' of
      GREATER => T (l, y, update r x)
    | LESS    => T (update l x, y, r)
    | EQUAL   => T (l, x ,r)

fun fromList xs = foldl (fn (x, t) => update t x) empty xs

fun splitFirst E = raise Empty
  | splitFirst (T (E, x, r)) = (x, r)
  | splitFirst (T (l, x, r)) =
    let
      val (y, l') = splitFirst l
    in
      (y, T (l', x, r))
    end

fun splitLast E = raise Empty
  | splitLast (T (l, x, E)) = (x, l)
  | splitLast (T (l, x, r)) =
    let
      val (y, r') = splitLast r
    in
      (y, T (l, x, r'))
    end

(* Crude balancing *)
val goLeft = ref true
fun split t = if (!goLeft before goLeft := not (!goLeft)) then
                splitFirst t
              else
                splitLast t

fun delete E _ = E
  | delete (T (l, y as (k', _), r)) k =
    case Key.compare k k' of
      GREATER => T (l, y, delete r k)
    | LESS    => T (delete l k, y, r)
    | EQUAL   =>
      let
        val (y', r') = splitFirst r
      in
        T (l, y', r')
      end

fun remove E _ = raise Domain
  | remove (T (l, y as (k', v'), r)) k =
    case Key.compare k k' of
      GREATER =>
      let
        val (v, r') = remove r k
      in
        (v, T (l, y, r'))
      end
    | LESS    =>
      let
        val (v, l') = remove l k
      in
        (v, T (l', y, r))
      end
    | EQUAL   =>
      let
        val (y', r') = splitFirst r
      in
        (v', T (l, y', r'))
      end

fun modify f E _ = raise Domain
  | modify f (T (l, y as (k', v'), r)) k =
    case Key.compare k k' of
      GREATER => T (l, y, modify f r k)
    | LESS    => T (modify f l k, y, r)
    | EQUAL   => T (l, (k', f v'), r)

fun lookup E _ = NONE
  | lookup (T (l, y as (k', v'), r)) k =
    case Key.compare k k' of
      GREATER => lookup r k
    | LESS    => lookup l k
    | EQUAL   => SOME v'

fun inDomain E _ = false
  | inDomain (T (l, y as (k', _), r)) k =
    case Key.compare k k' of
      GREATER => inDomain r k
    | LESS    => inDomain l k
    | EQUAL   => true

fun isEmpty E = true
  | isEmpty _ = false

fun size E = 0
  | size (T (l, _, r)) = 1 + size l + size r

fun toList E = nil
  | toList (T (l, x, r)) = toList l @ x :: toList r

fun domain E = nil
  | domain (T (l, (k, _), r)) = domain l @ k :: domain r

fun range E = nil
  | range (T (l, (_, v), r)) = range l @ v :: range r

fun first E = raise Empty
  | first (T (E, (_, v), _)) = v
  | first (T (l, _, _)) = first l
fun firsti E = raise Empty
  | firsti (T (E, x, _)) = x
  | firsti (T (l, _, _)) = firsti l

fun last E = raise Empty
  | last (T (_, (_, v), E)) = v
  | last (T (_, _, r)) = last r
fun lasti E = raise Empty
  | lasti (T (_, x, E)) = x
  | lasti (T (_, _, r)) = lasti r

fun collate _ = die "collate"
fun partition _ = die "partition"
fun partitioni _ = die "partitioni"
fun filter _ = die "filter"
fun filteri _ = die "filteri"
fun exists _ = die "exists"
fun existsi _ = die "existsi"

fun all _ E = true
  | all p (T (l, (_, v), r)) =
    p v andalso all p l andalso all p r

fun alli _ E = true
  | alli p (T (l, (k, v), r)) =
    p (k, v) andalso alli p l andalso alli p r

fun find _ = die "find"
fun findi _ = die "findi"
fun app _ E = ()
  | app f (T (l, (k, v), r)) =
    (app f l ; f v ; app f r)
fun appi _ E = ()
  | appi f (T (l, (k, v), r)) =
    (appi f l ; f (k, v) ; appi f r)
fun map _ E = E
  | map f (T (l, (k, v), r)) = T (map f l, (k, f v), map f r)
fun mapi _ E = E
  | mapi f (T (l, (k, v), r)) = T (mapi f l, (k, f (k, v)), mapi f r)
fun mapPartial _ = die "mapPartial"
fun mapPartiali _ = die "mapPartiali"
fun foldl _ b E = b
  | foldl f b (T (l, (_, v), r)) =
    foldl f (foldl f (f (v, b)) l) r
fun foldli _ = die "foldli"
fun foldr _ = die "foldr"
fun foldri _ = die "foldri"
fun union _ = die "union"
fun unioni _ = die "unioni"
fun inter _ = die "inter"
fun interi _ = die "interi"
fun merge _ = die "merge"
fun mergi _ = die "mergi"
fun plus a b = List.foldl (fn (x, m) => update m x) a (toList b)

fun toString keyToString valueToString t =
    let
      fun toString' E = ""
        | toString' (T (l, (k, v), r)) =
          (toString' l) ^ (keyToString k) ^ ": " ^ (valueToString v) ^ "\n" ^ (toString' r)
    in
      toString' t
    end

end
(* Default OrderedMap implementation *)

(* OrderedMapFn : Ordering -> OrderedMap *)

functor OrderedMapFn (Key : Ordered) = UnbalancedOrderedMapFn (Key)
(* functor OrderedMapFn (Key : Ordered) = SMLNJMapFn (Key) *)
(* A dictionary is a mapping from strings to some domain *)

structure Dictionary = OrderedMapFn (struct
                                     type t = string
                                     fun compare x y = String.compare (x, y)
                                     end)
structure IntMap = OrderedMapFn (struct
                                 type t = int
                                 fun compare x y = Int.compare (x, y)
                                 end)
structure CharMap = OrderedMapFn
                      (struct
                       type t = char
                       fun compare x y = Char.compare (x, y)
                       end)
val id = General.id
val const = General.const
val flipc = General.flipc
val flip = General.flip
val \< = General.\<
val \> = General.\>
val ^* = General.^*
val $ = General.$
val <- = String.<-
val curry = General.curry
val uncurry = General.uncurry
val pair = General.pair
val curry3 = General.curry3
val uncurry3 = General.uncurry3
val triple = General.triple
val curry4 = General.curry4
val uncurry4 = General.uncurry4
val quadruple = General.quadruple
val to = General.to
val inc = General.inc
val dec = General.dec
infix 5 ^* to <-
infix 4 \< \>
infixr 3 $

val println = TextIO.println
val system = OS.Process.system
val exit = OS.Process.exit
val ltoi = Int.fromLarge
val itol = Int.toLarge

datatype either = datatype Either.t
exception Either = Either.Either
val ofLeft = Either.ofLeft
val ofRight = Either.ofRight
(* val lefts = Either.lefts *)
(* val rights = Either.rights *)
val either = Either.either

(* val first = Arrow.first *)
(* val second = Arrow.second *)
(* val ** = Arrow.** *)
(* val && = Arrow.&& *)
(* val left = Arrow.left *)
(* val right = Arrow.right *)
(* val ++ = Arrow.++ *)
(* val || = Arrow.|| *)
(* val >> = Arrow.>> *)
(* val << = Arrow.<< *)
(* val loop = Arrow.loop *)
(* infix 4 ** && ++ || *)
(* infix 3 << >> *)

val fst = Pair.fst
val snd = Pair.snd
signature Pretty =
sig
  type t
  type line = int * string

  val pretty : int option -> t -> string
  (* The flattening never contains line breaks *)
  val flatten : t -> t
  val fold : (line * 'a -> 'a) -> 'a -> int option -> t -> 'a
  val linearize : int option -> t -> line list

  val empty : t (* empty = txt "" *)
  val txt : string -> t


  (* New line and indent or a space depending on grouping. *)
  val ln : t

  (* New line and indent or nothing depending on grouping. *)
  val brk : t


  (* Add a number of columns to the indentation level. *)
  val nest : int -> t -> t

  (* Alternative outputs: The narrow alternative is just the given document. The
   * wide one is generated by treating new lines and line breaks as described
   * above and for each choice in the document taking the wide one. *)
  val group : t -> t

  (* Alternative outputs: The first component is a wide variant, the second is a
   * narrow one.
   * Note: The wide variant is flattened, so it will not contain line breaks. *)
  val choice : t * t -> t

  (* Primarily useful for specifying the narrow alternative to choice; If the
   * wide variant fits, the narrow one is never evaluated. *)
  val delay : t Lazy.thunk -> t


  (* Takes a function that given the current column generates a document. *)
  val column : (int -> t) -> t

  (* Takes a function that given the current nesting level generates a document. *)
  val nesting : (int -> t) -> t

  (* Takes a function that given the desired maximal printing width generates a
   * document *)
  val max : (int option -> t) -> t

  val ^^ : t * t -> t (* Join horizontally. *)
end
structure Pretty :> Pretty =
struct
open Lazy General
datatype t' =
         Empty
       | Join    of t * t
       | Line    of bool
       | Nest    of int * t
       | Text    of string
       | Choice  of t * t
       | Nesting of int -> t
       | Column  of int -> t
       | Max     of int option -> t
withtype t = t' Lazy.t
type line = int * string

val empty = eager Empty
val txt = eager o Text

val ln = eager (Line true)
val brk = eager (Line false)
val column = eager o Column
val nesting = eager o Nesting
val max = eager o Max

infix ^^

val op^^ = eager o Join

fun nest n = eager o curry Nest n

fun flatten doc =
    delay
      (fn _ =>
          case force doc of
            Empty         => doc
          | Join (l, r)   => flatten l ^^ flatten r
          | Nest (i, doc) => nest i (flatten doc)
          | Text _        => doc
          | Line b        => if b then txt " " else empty
          | Choice (w, _) => w
          | Column f      => column (flatten o f)
          | Nesting f     => nesting (flatten o f)
          | Max f         => max (flatten o f)
      )

fun choice (w, n) =
    eager (Choice (flatten w, n))
fun group d = choice (d, d)

fun linearize max doc =
    let
      datatype t' =
               Nil
             | Print of string * t
             | Linefeed of int * t
      withtype t = t' Lazy.t

      fun lin doc =
          let
            fun loop (i, acc, doc) =
                case force doc of
                  Nil                => [(i, acc)]
                | Print (str, doc)   => loop (i, acc ^ str, doc)
                | Linefeed (i', doc) => (i, acc) :: loop (i', "", doc)
          in
            loop (0, "", doc)
          end

      fun fits used doc =
          case max of
            NONE   => true
          | SOME c => used <= c andalso
                      case force doc of
                        Print (str, doc) => fits (used + size str) doc
                      | _                => true

      fun best used wl =
          delay
            (fn _ =>
                case wl of
                  nil => eager Nil
                | (nest, doc) :: rest =>
                  case force doc of
                    Empty         =>
                    best used rest
                  | Join (l, r)   =>
                    best used ((nest, l) :: (nest, r) :: rest)
                  | Nest (i, doc) =>
                    best used ((nest + i, doc) :: rest)
                  | Text str      =>
                    eager (Print (str, best (used + size str) rest))
                  | Line _        =>
                    eager (Linefeed (nest, best nest rest))
                  | Choice (w, n) =>
                    let
                      val w = best used ((nest, w) :: rest)
                    in
                      if fits used w then
                        w
                      else
                        best used ((nest, n) :: rest)
                    end
                  | Column f      =>
                    best used ((nest, f used) :: rest)
                  | Nesting f     =>
                    best used ((nest, f nest) :: rest)
                  | Max f         =>
                    best used ((nest, f max) :: rest)
            )
    in
      lin (best 0 [(0, doc)])
    end

fun fold f s max doc = foldl f s (linearize max doc)

local
  fun strs nil = nil
    | strs ((_, "") :: ls) = "\n" :: strs ls
    | strs [(i, s)] = [String.spaces i, s]
    | strs ((i, s) :: ls) = String.spaces i :: s :: "\n" :: strs ls
in
fun pretty n d = String.concat (strs (linearize n d))
end
end
(* The way it works, is that Layout is an extension of Pretty. So every
 * declaration in Pretty is also in Layout. Think of it as Pretty defining the
 * basic operations and Layout building sugar on top of that.
 *)

(* TODO
 * Make a suite of examples.
 *)

signature Layout =
sig
  (* infix ^^ ++ \ & \\ && *)
  include Pretty

  (* Prints to standard out (with an extra \n). Takes an optional max width. *)
  val println : int option -> t -> unit

  (* A space if the output fits, new line and indent if it doesn't. *)
  val softln : t

  (* Nothing if the output fits, new line and indent if it doesn't. *)
  val softbrk : t

  (* Replaces all spaces with softln. *)
  val softtxt : string -> t

  (* Like softtxt but preprends two spaces. *)
  val paragraph : string -> t

  (* Converts a preformatted text into a document. A newline character separates
   * paragraphs and the following number of spaces determine the next paragraphs
   * indentation. So it basically does what you would expect. *)
  val str : string -> t

  (* Takes the desired bullet as a string *)
  val itemize : string -> t list -> t

  datatype enumeration = Number | Letter | Roman | CapitalLetter | CapitalRoman
  (* Takes an enumeration schema (which is a string to print before each
   * enumeration, a kind of enumeration, and a string to print after) and an
   * optional starting number (default is 1). *)
  val enumerate : string * enumeration * string -> int option -> t list -> t

  (* Double spaces after the word being described, following lines indented. *)
  val description : (string * t) list -> t

  (* Places one document besides another. The first argument determines the
   * spacing *)
  val besides : int -> t * t -> t

  (* Flushes to the right if a maximum width is given. Does nothing otherwise *)
  val flushRight : t -> t

  (* Indents a document. The indented document should follow a line break. *)
  val indent : int -> t -> t

  (* As align except the lines following the first one is indented further. *)
  val hang : int -> t -> t

  (* Aligns the lines of a document vertically (modulo internal indentation). *)
  val align : t -> t

  (* Takes a function that given the printed width of a document generates a
   * second document. The two documents are then joined. *)
  val width : (int -> t) -> t -> t

  (* Takes function that given the columns left until the desired maximum width
   * is reached produces a document *)
  val left : (int option -> t) -> t

  (* Takes a desired width and a document. Appends spaces if the document is
   * narrower, or inserts a line break and indents if it isn't. *)
  val fillBreak : int -> t -> t

  (* Takes a desired width and appends spaces to a document if it is narrower or
   * does nothing if it isn't. *)
  val fill : int -> t -> t

  val ++ : t * t -> t (* l ++ r = l ^^ txt " " ^^ r *)
  val \  : t * t -> t (* l \\ r = l ^^ ln ^^ r      *)
  val &  : t * t -> t (* l \ r  = l ^^ softln ^^ r  *)
  val \\ : t * t -> t (* l \\ r = l ^^ brk ^^ r     *)
  val && : t * t -> t (* l && r = l ^^ softbrk ^^ r *)

  (* Lays out its elements horizontally *or* vertically; nothing in between. *)
  val sep : t list -> t (* sep = group o vsep *)
  val cat : t list -> t (* cat = group o vcat *)

  (* Concatenates a document to the right of each document in the list, except
   * the last one. *)
  val punctuate : t -> t list -> t list

  (* Seperate (As concatenate but always puts something between the documents,
   * eg. a space or a new line). *)
  val hsep : t list -> t (* With txt " " *)
  val vsep : t list -> t (* With nl      *)
  (* Lays out as much as it can before a line break *)
  val fsep : t list -> t (* With softnl  *)

  (* Concatenate. *)
  val hcat : t list -> t (* With empty   *)
  val vcat : t list -> t (* With brk     *)
  (* Lays out as much as it can before a line break *)
  val fcat : t list -> t (* With softbrk *)


  val enclose : t * t -> t -> t (* enclose (l, r) d = l ^^ d ^^ r *)
  val plings : t -> t           (* = enclose (pling, pling)       *)
  val quotes : t -> t           (* = enclose (quote, quote)       *)
  val parens : t -> t           (* = enclose (lparen, rparen)     *)
  val angles : t -> t           (* = enclose (langle, rangle)     *)
  val braces : t -> t           (* = enclose (lbrace, rbrace)     *)
  val brackets : t -> t         (* = enclose (lbracket, rbracket) *)

  val spaces : int -> t

  val lparen : t    (* = txt "("  *)
  val rparen : t    (* = txt ")"  *)
  val langle : t    (* = txt "<"  *)
  val rangle : t    (* = txt ">"  *)
  val lbrace : t    (* = txt "{"  *)
  val rbrace : t    (* = txt "}"  *)
  val lbracket : t  (* = txt "["  *)
  val rbracket : t  (* = txt "]"  *)
  val pling : t     (* = txt "'"  *)
  val quote : t     (* = txt "\"" *)
  val semi : t      (* = txt ";"  *)
  val colon : t     (* = txt ":"  *)
  val comma : t     (* = txt ","  *)
  val space : t     (* = txt " "  *)
  val dot : t       (* = txt "."  *)
  val dash : t      (* = txt "-"  *)
  val sharp : t     (* = txt "#"  *)
  val percent : t   (* = txt "%"  *)
  val dollar : t    (* = txt "$"  *)
  val ampersand : t (* = txt "&"  *)
  val slash : t     (* = txt "/"  *)
  val backslash : t (* = txt "\\" *)
  val eq : t        (* = txt "="  *)
  val tilde : t     (* = txt "~"  *)
  val asterisk : t  (* = txt "*"  *)
  val bar : t       (* = txt "|"  *)

  val chr : char -> t
  val int : int -> t
  val real : real -> t
  val bool : bool -> t
  val option : ('a -> string) -> 'a option -> t
end
structure Layout :> Layout =
struct
open General infix 2 $
open Pretty infix ^^ ++ \ & \\ &&

fun die s = raise Fail ("Layout: " ^ s)

datatype enumeration = Number | Letter | Roman | CapitalLetter | CapitalRoman

fun println n d = TextIO.println (pretty n d)

val chr = txt o str
val int = txt o Show.int
val real = txt o Show.real
val bool = txt o Show.bool
fun option f = txt o Show.option f

val spaces = txt o String.spaces

val lparen = txt "("
val rparen = txt ")"
val langle = txt "<"
val rangle = txt ">"
val lbrace = txt "{"
val rbrace = txt "}"
val lbracket = txt "["
val rbracket = txt "]"
val pling = txt "'"
val quote = txt "\""
val semi = txt ";"
val colon = txt ":"
val comma = txt ","
val space = txt " "
val dot = txt "."
val dash = txt "-"
val sharp = txt "#"
val percent = txt "%"
val dollar = txt "$"
val ampersand = txt "&"
val slash = txt "/"
val backslash = txt "\\"
val eq = txt "="
val tilde = txt "~"
val asterisk = txt "*"
val bar = txt "|"

fun punctuate sep ds =
    let
      fun loop nil = nil
        | loop [d] = [d]
        | loop (d :: ds) = (d ^^ sep) :: loop ds
    in
      loop ds
    end

fun align d = column (fn k => nesting (fn i => nest (k - i) d))
fun hang i d = align (nest i d)
fun indent i d = hang i (spaces i ^^ d)
fun width f d = column (fn l => d ^^ column (fn r => f (r - l)))
fun left f = column (fn c => max $ f o Option.map (fn m => m - c))

fun fill f =
    width (fn w =>
              if f > w then
                spaces (f - w)
              else
                empty
          )
fun fillBreak f =
    width (fn w =>
              if f >= w then
                spaces (f - w)
              else
                nest f brk
          )

val softln = group ln
val softbrk = group brk

local
  fun mk sep (l, r) = l ^^ sep ^^ r
in
val op++ = mk space
val op\  = mk ln
val op&  = mk softln
val op\\ = mk brk
val op&& = mk softbrk
end

local
  fun mk sep ds =
      case rev ds of
        nil     => empty
      | d :: ds => foldl sep d ds
in
val hsep = mk op++
val vsep = mk op\
val fsep = mk op&
val hcat = mk op^^
val vcat = mk op\\
val fcat = mk op&&
end

val sep = group o vsep
val cat = group o vcat

fun enclose (l, r) d = l ^^ d ^^ r
val plings   = enclose (pling, pling)
val quotes   = enclose (quote, quote)
val parens   = enclose (lparen, rparen)
val angles   = enclose (langle, rangle)
val braces   = enclose (lbrace, rbrace)
val brackets = enclose (lbracket, rbracket)

fun softtxt s =
    (fsep o
     map txt o
     String.fields Char.isSpace
    ) s
val paragraph = curry op^^ (spaces 2) o softtxt

fun str s =
    let
      datatype bullet = Text of string
                      | Numbered of string * int * string
(* TODO: We could really use a parser combinator library here. The code below
 * could be a starting point. Also review Parsec and the parser on JLouis' blog.
 *)
      fun bullet s =
          let
            fun or nil ss = NONE
              | or (r :: rs) ss =
                case r ss of
                  SOME x => SOME x
                | NONE   => or rs ss

            fun cat rs ss =
                let
                  fun loop (nil, ss) = SOME ("", ss)
                    | loop (r :: rs, ss) =
                      case r ss of
                        NONE => NONE
                      | SOME (s, ss) =>
                        case loop (rs, ss) of
                          NONE => NONE
                        | SOME (s', ss) => SOME (s ^ s', ss)
                in
                  loop (rs, ss)
                end

            val number = Int.scan StringCvt.DEC Substring.getc
            fun string s ss =
                let
                  fun loop (ss, nil) = SOME (s, ss)
                    | loop (ss, c :: cs) =
                      case Substring.getc ss of
                        SOME (c', ss') =>
                        if c' = c then
                          loop (ss', cs)
                        else
                          NONE
                      | NONE => NONE
                in
                  loop (ss, explode s)
                end
            fun ws ss =
                let
                  fun loop ss' =
                      case Substring.getc ss' of
                        SOME (c, ss') =>
                        if c = #" " then
                          case loop ss' of
                            SOME (s, ss') => SOME (" " ^ s, ss')
                          | NONE => SOME (" ", ss')
                        else
                          SOME ("", ss)
                      | NONE => NONE
                in
                  loop ss
                end
            val empty = string ""
            val AD = string "AD"
            val Ad = string "Ad"
            val ad = string "ad"
            val dot = string "."
            val colon = string ":"

            fun numbered ss =
                case cat [or [AD, Ad, ad, empty], ws] ss of
                  NONE => NONE
                | SOME (l, ss) =>
                  case number ss of
                    NONE => NONE
                  | SOME (n, ss) =>
                    case or [cat [ws, or [dot, colon]], empty] ss of
                      NONE => NONE
                    | SOME (r, ss) =>
                      SOME (Numbered (l, n, r), ss)

            fun text ss =
                case Substring.getc ss of
                  SOME (c, ss) =>
                  if c = #"o" orelse Char.isPunct c then
                    SOME (Text (String.str c), ss)
                  else
                    NONE
                | NONE => NONE
          in
            case or [numbered, text] s of
              SOME (b, s) => (SOME b, s)
            | NONE        => (NONE, s)
          end
      fun next (Text s) = Text s
        | next (Numbered (l, n, r)) = Numbered (l, n + 1, r)
      fun bulletToString b =
          case b of
            Text s => s
          | Numbered (l, n, r) => l ^ Int.toString n ^ r

      datatype t = Par of int * substring
                 | List of int * (string * t list) list

      fun trees ls =
          let
            fun loop (_, nil) = (nil, nil)
              | loop (i', ll as (i, l) :: ls) =
                if i' <= i then
                  case bullet l of
                    (NONE, _) =>
                    let
                      val (ts, ls) = loop (i', ls)
                    in
                      (Par (i - i', l) :: ts, ls)
                    end
                  | (SOME b, _) =>
                    let
                      fun loopi (_, _, nil) = (nil, nil)
                        | loopi (i'', b', ll as (i, l) :: ls) =
                          case (i'' = i, bullet l) of
                            (true, (SOME b, l)) =>
                            let
                              val b = if b = Text "." then next b' else b
                              val bs = bulletToString b
                              val (item, ls) = loop (i + size bs + 1, ls)
                              val (items, ls) = loopi (i, b, ls)
                            in
                              ((bs, Par (0, l) :: item) :: items, ls)
                            end
                          | _ => (nil, ll)
                      val (lst, ls) = loopi (i, b, ll)
                      val (ts, ls) = loop (i', ls)
                    in
                      (List (i - i', lst) :: ts, ls)
                    end
                else
                  (nil, ll)
            val (ts, _) = loop (0, ls)
          in
            ts
          end

      val softtxt = softtxt o Substring.string

      fun doc ts =
          let
            fun one (Par (i, s)) =
                spaces i ^^ nest i (softtxt s)
              | one (List (i, items)) =
                let
                  val width = foldl Int.max 0 (map (size o Pair.fst) items)
                in
                  vcat
                    (map
                       (fn (b, ts) =>
                           spaces i ^^ (fill width (txt b)) ^^
                                  nest (width + i + 1) (many ts)
                       ) items
                    )
                end
            and many ts = vcat (map one ts)
          in
            many ts
          end
    in
      doc o
      trees o
      map (Pair.map (Substring.size, id) o
           Substring.splitl Char.isSpace
          ) o
      Substring.fields (curry op= #"\n") $
      Substring.full s
    end

fun besides spc (l, r) =
    left
      (fn w =>
          let
            val w = Option.map (fn w => (w - spc) div 2) w
            val ls = linearize w l
            val rs = linearize w r
            val lw = foldl Int.max 0 $ map (fn (i, s) => i + size s) ls
            fun stitch (nil, (i, s) :: rs) =
                [spaces (lw + i + spc), txt s] :: stitch (nil, rs)
              | stitch ((i, s) :: ls, nil) =
                [spaces i, txt s] :: stitch (ls, nil)
              | stitch ((il, sl) :: ls, (ir, sr) :: rs) =
                [spaces il, txt sl, spaces (lw - il - size sl + spc),
                 spaces ir, txt sr] :: stitch (ls, rs)
              | stitch _ = nil
          in
            align o vcat o map hcat $ stitch (ls, rs)
          end
      )

fun flushRight d =
    left
      (fn NONE   => d
        | SOME w =>
          let
            val ls = linearize (SOME w) d
          in
            align o vcat $ map (fn (_, s) => spaces (w - size s) ^^ txt s) ls
          end
      )
fun itemize bullet ds =
    vcat $ map (curry op^^ (txt bullet ^^ space) o align) ds

local
  val number = Int.toString
  fun letter 1 = "a"
    | letter n =
      let
        val sigma = "abcdefghijklmnopqrstuvwxyz"
        val l = size sigma
        fun loop 0 = nil
          | loop n = String.sub (sigma, n mod l) :: loop (n div l)
      in
        implode o rev $ loop (n - 1)
      end
  local
    fun toChar x =
        case x of
          1000 => #"m"
        |  500 => #"d"
        |  100 => #"c"
        |   50 => #"l"
        |   10 => #"x"
        |    5 => #"v"
        |    1 => #"i"
        | _    => die "enumbullets.roman"
    fun toRoman x =
        let
          val rs = [1000, 500, 100, 50, 10, 5, 1]
          val rsr = rev rs
          fun subtract (x, y :: ys) =
              if x + y >= 0 then
                y
              else
                subtract (x, ys)
            | subtract _ = die "enumbullets.roman"

          fun loop (0, _) = nil
            | loop (x, yl as y :: ys) =
              if x >= y then
                y :: loop (x - y, yl)
              else
                (* Don't ask - it works *)
                if x >= y * (9 - y div hd ys mod 2) div 10 then
                  let
                    val z = subtract (x - y, rsr)
                  in
                    z :: y :: loop (x - y + z, ys)
                  end
                else
                  loop (x, ys)
            | loop _ = die "enumbullets.roman"
        in
          loop (x, rs)
        end
  in
  val roman = implode o map toChar o toRoman
  end
  fun enum style =
      case style of
        Number        => number
      | Letter        => letter
      | Roman         => roman
      | CapitalLetter => String.map Char.toUpper o letter
      | CapitalRoman  => String.map Char.toUpper o roman
in
fun enumerate (l, style, r) start ds =
    let
      val enum = enum style
      val start = Option.getOpt (start, 1)
      val bullets = List.tabulate
                      (length ds, fn n => l ^ enum (start + n) ^ r)
      val w = foldl Int.max 0 $ map size bullets
    in
      vcat o map (fn (b, s) => fill w $ txt b ++ align s)
                 $ ListPair.zip (bullets, ds)
    end
end

fun description items = vcat $ map (fn (s, d) => txt s ++ space ^^ nest 2 d) items
end
