structure TestDSL :> TestDSL =
struct
infixr 0 slut er
infix 1 afproev note hvor og
infix 2 ? indeholder
infix 3 ::: & ==> ~~> !!!
infix 4 eller

type ('a, 'b) folder = ('a * 'b) * 'b -> 'a * 'b
type exn' = string option

fun saet e = e
fun e er t = Test.udskriv (e, t)

val heltslut = nil
val op slut = op::

val E = Lazy.eager
val L = Lazy.lazy
val F = Lazy.force

fun opgave nr navn = ((nr, navn, NONE), nil)
fun ((nr, navn, n), l) note b =
    ((nr,
      navn,
      case n of
        NONE => SOME b
      | SOME b' => SOME (b' ^ "\n" ^ b)
     ), l)
fun x hvor _ = x
fun x og _ = x
fun n indeholder c = TextIO.writeFile (Constants.FILER ^ "/" ^ n) c
fun fil n = Constants.FILER ^ "/" ^ n

fun (opg, fs) afproev f = (opg, L (fn _ => Test.tjek f) :: fs)

fun (f, cs) ? c = (f, c @ cs)

fun a ::: b = [(a, b)]

fun aekvivalens s f x _ = (fn y => f (F y, x), [Test.Vaerdi (x, s)])
fun praedikat s f _ = (f o F, [Test.Beskrivelse s])
fun reference f ind =
    let
      val r' = L (fn _ => f ind)
      datatype 'a res = V of 'a
                      | E of string
      fun eval x = V $ F x handle e => E $ General.exnName e
    in
      (fn r => eval r = eval r',
       [Test.Vaerdi (F r', "_ (referenceimplementering)")
        handle e => Test.Beskrivelse $ General.exnName e ^
                    " (referenceimplementering)"
       ]
      )
    end

fun automagisk n r g =
    List.tabulate (n, fn _ => (Gen.run g, reference r))

fun (x eller y) ind =
    let
      val (f, a) = x ind
      val (g, b) = y ind
    in
      (fn x => f x orelse g x handle _ => g x, a @ b)
    end

fun lig x =
    aekvivalens
      "_"
      op=
      x

fun circa x =
    aekvivalens
      "_ modulo epsilon"
      (fn (x, y) => let val epsilon = 0.00001 in Real.abs (x - y) < epsilon end)
      x

fun permutation x =
    aekvivalens
      "en permutation af _"
      (fn (x, y) => Set.equal (Set.fromList x) (Set.fromList y))
      x

fun delliste x =
    aekvivalens
      "en delliste af _"
      let
        fun prefix (nil, _) = true
          | prefix (_, nil) = false
          | prefix (x :: xs, y :: ys) = x = y andalso prefix (xs, ys)
        fun loop (xs, ys as _ :: ys') = prefix (xs, ys) orelse loop (xs, ys')
          | loop _ = false
      in
        loop
      end
      x

fun blandt xs =
    case map lig xs of
      x :: xs => foldl (flip op eller) x xs
    | _ => raise Fail "brug af 'blandt' med tom liste"

fun a ==> b = a ::: lig b
fun a ~~> b = a ::: circa b

datatype exn' = Bind
              | Chr
              | Div
              | Domain
              | Empty
              | Fail
              | Interrupt
              | Match
              | Option
              | Overflow
              | Size
              | Subscript
              | Out_of_memory
              | undtagelse
fun which e =
    case e of
      Bind => SOME "Bind"
    | Chr => SOME "Chr"
    | Div => SOME "Div"
    | Domain => SOME "Domain"
    | Empty => SOME "Empty"
    | Fail => SOME "Fail"
    | Interrupt => SOME "Interrupt"
    | Match => SOME "Match"
    | Option => SOME "Option"
    | Overflow => SOME "Overflow"
    | Size => SOME "Size"
    | Subscript => SOME "Subscript"
    | Out_of_memory => SOME "Out_Of_Memory"
    | undtagelse => NONE

fun kaster e _ = (fn r => (F r ; false)
                     handle e' =>
                            case which e of
                              SOME e => General.exnName e' = e orelse raise e'
                            | NONE   => true
                , [Test.Beskrivelse (case which e of
                                       SOME e => e
                                     | NONE => "en undtagelse")
                  ]
                 )

fun a !!! b = a ::: kaster b

end
