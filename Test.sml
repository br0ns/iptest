structure Test :> Test =
struct
exception Tom

datatype 'a forventning = Vaerdi of 'a * string
                        | Beskrivelse of string

datatype resultat' = Udeladt
                   | Resultater of tilfaelde list
     and tilfaelde = OK
                   | Modeksempel of Layout.t * Layout.t * Layout.t list
type resultat = string * resultat'

type ('a, 'b) funktion = string * ('a -> 'b) * 'a Vis.t * 'b Vis.t
type ('a, 'b) egenskab = 'a -> ('b Lazy.t -> bool) * 'b forventning list
(* datatype ('a, 'b) tilfaelde *)
(*   = Manuel of 'a * ('a, 'b) egenskab *)
(*   | Auto   of 'a Gen.t * ('a -> 'b) *)
type ('a, 'b) tilfaelde = ('a * ('a, 'b) egenskab) list
type ('a, 'b) afproevning = ('a, 'b) funktion * ('a, 'b) tilfaelde
type opgave = (string * string * string option) * resultat Lazy.t list

open Layout
infix ^^ ++ \ & \\ &&
val indent = indent Constants.INDRYK
val L = Lazy.lazy
val F = Lazy.force


fun reference f ind =
    let
      val r' = L (fn _ => f ind)
      datatype 'a res = V of 'a
                      | E of string
      fun eval x = V $ F x handle e => E $ General.exnName e
    in
      (fn r => eval r = eval r',
       [Vaerdi (F r', "_ (referenceimplementering)")
        handle e => Beskrivelse $ General.exnName e ^
                    " (referenceimplementering)"
       ]
      )
    end

fun tjek  ((n, f, va, vr), tilfs) =
    let
      val pbar = ref $ ProgressBar.init Constants.KOLONNER
      val delta = 1.0 / Real.fromInt (length tilfs)
      fun indsaet x s =
          let
            fun loop ss =
                case ss of
                  [s] => s
                | s :: ss => s && x && loop ss
                | _ => txt ""
          in
            loop $ map softtxt $ String.fields (#"_" \< op=) s
          end
      fun en (ind, egenskab) =
          let
            val (testf, forventninger) = egenskab ind
            val _ = pbar := ProgressBar.progress (!pbar) delta
            (* val _ = TextIO.print "." *)
            val _ = Benchmark.start ()
            val ud = L (fn _ => f ind)
            fun forventning f =
                case f of
                  Vaerdi (x, s) => indsaet (vr x) s
                | Beskrivelse s => indsaet (va ind) s
            fun tjekUdeladt ud =
                (F ud ; ()) handle Tom => raise Tom | _ => ()
          in
            (if (tjekUdeladt ud ; testf ud) then
               OK
             else
               Modeksempel (va ind, vr $ F ud, map forventning forventninger)
            ) handle
            Tom => raise Tom
          | e =>
            Modeksempel
              (va ind,
               softtxt
                 (case e of
                    Fail s => "\"" ^ s ^ "\" (Fail)"
                  | Interrupt =>
                    (TextIO.println $ "\nAfbrød '" ^ n ^ "'!";
                     Benchmark.stop ();
                     "Kørsel afbrudt efter " ^
                     (Show.int $ Time.toMilliseconds $ #tot $ Benchmark.time ()) ^
                     "ms (Interrupt)")
                  | Out_of_memory => "Out_of_memory (uendelig løkke?)"
                  | Io { function, name, cause } =>
                      "Io { function : "^function^
                         ", name : "^name^
                         ", cause : "^ exnMessage cause ^" }"
                  | _ => General.exnName e
                 ),
               map forventning forventninger
              )
          end
    in
      TextIO.println ("\n~~~~~ Afprøver %" <- n)
    ; (n, Resultater $ map en $ rev tilfs handle Tom => Udeladt)
      before ProgressBar.progress (!pbar) 1.0
    end

fun udskriv (saetnavn, opgs) =
    let
      val _ = TextIO.print "\n~~~~~~~~~~~~~ Starter afprøvning ~~~~~~~~~~~~~"
      fun oprems fs =
          let
            val eller = txt "     eller"
            val komma = txt "         ,"
            val rec loop =
             fn (_, nil) => empty
              | (_, [x]) => x
              | (1, xs) => txt "... (yderligere" & int (length xs) & txt "ikke vist)"
              | (_, [x, y]) => x \ eller & y
              | (n, x :: xs) => x \ komma & loop (n - 1, xs)
          in
            loop (Config.FORVENTNINGER, fs)
          end
      fun funktion (n, r) =
          txt "Funktion" ++ plings $ txt n ^^ colon &
              (case r of
                 Udeladt => txt "Udeladt."
               | Resultater r =>
                 let
                   val meksr =
                       List.mapPartial
                         (fn Modeksempel (x, y, z) =>
                             SOME (txt "Inddata  :" & align x \
                                       txt "Resultat :" & align y \
                                       txt "Forventet:" & oprems $ map align z
                                  )
                           | _ => NONE) r
                   val lmeksr' = length meksr
                   val lmeksr = Int.min (lmeksr', Constants.MODEKSEMPLER)
                 in
                   if lmeksr = 0 then
                     txt "OK" ++ parens $ int $ length $ r ^^ dot
                   else
                     txt "Viser" ++ int lmeksr ++ txt "af" ++ int lmeksr' ++
                         txt "modeksemp" ^^
                         (if lmeksr' = 1 then txt "el" else txt "ler") ++
                         parens $ int $ length $ r ^^ dot \
                         indent $ vsep $ punctuate ln $ List.take (meksr, lmeksr)
                 end
              )
      fun opgave ((navn, beskriv, note), fs) =
          txt "Opgave" ++ txt navn ++
              (if beskriv <> "" then dash ++ txt beskriv else empty) ^^ dot \
              (case note of
                 SOME n => str n ^^ ln
               | NONE => empty) ^^
              indent $ Layout.vsep $ punctuate ln $ map (funktion o F) $ rev fs
      val rapport =
          pretty
            (SOME Constants.KOLONNER)
            (txt $ "=== Afprøvning '%' af '%' ==="
                 <- saetnavn
                 <- OS.Path.file Constants.BESVARELSE \
                 vsep $ punctuate ln $ map opgave opgs
            )
      val header = Constants.TEST ^ "/header.txt"
      val _ = TextIO.writeFile
                (Constants.BESVARELSE ^ ".rapport")
                ((if OS.FileSys.access (header, nil) then
                    TextIO.readFile header
                  else
                    ""
                 ) ^
                 rapport
                )
    in
      TextIO.println "" ;
      TextIO.println rapport
    end
end
