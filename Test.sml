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

type ('a, 'b) funktion = string * ('a -> 'b) * ('a -> Layout.t) * ('b -> Layout.t)
type ('a, 'b) egenskab = 'a -> ('b Lazy.t -> bool) * 'b forventning list
type ('a, 'b) afproevning = ('a, 'b) funktion * ('a * ('a, 'b) egenskab) list
type opgave = (string * string * string option) * resultat Lazy.t list

open Layout
infix ^^ ++ \ & \\ &&
val indent = indent Constants.INDRYK
val L = Lazy.lazy
val F = Lazy.force

fun tjek  ((n, f, va, vr), cs) =
    let
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
            val _ = TextIO.println $ "Afprøver '" ^ n ^ "'..."
            val _ = Benchmark.start ()
            val ud = L (fn _ => f ind)
            fun forventning f =
                case f of
                  Vaerdi (x, s) => indsaet (vr x) s
                | Beskrivelse s => indsaet (va ind) s
          in
            (if testf ud then
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
                  | _ => General.exnName e
                 ),
               map forventning forventninger
              )
          end
    in
      (n, Resultater $ map en $ rev cs handle Tom => Udeladt)
    end

fun udskriv (saetnavn, opgs) =
    let
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
          txt "Funktion" ++ plings $ txt n ^^ colon
              & (case r of
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
                       txt "Viser" ++ int lmeksr ++ txt "af" ++ int lmeksr' ++ txt "modeksemp" ^^
                           (if lmeksr' = 1 then txt "el" else txt "ler") ++
                           parens $ int $ length $ r ^^ dot \
                           indent $ vsep $ punctuate ln $ List.take (meksr, lmeksr)
                   end
                )
      fun opgave ((nr, navn, note), fs) =
          txt "Opgave" ++ txt nr ++ dash ++ txt navn ^^ dot \
              (case note of
                 SOME n => str n ^^ ln
               | NONE => empty) ^^
              indent $ Layout.vsep $ punctuate ln $ map (funktion o F) $ rev fs
      val rapport =
          pretty
            (SOME Constants.KOLONNER)
            (txt "===" ++ txt ("Afprøvning '%' af" <- saetnavn) ++
                 txt Constants.BESVARELSE ++ txt "===" \
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
