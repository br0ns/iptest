val use = fn f => use f handle _ => quit ()
;use "MyLib.sml";
;use "Config.sml";
;load "Random";

structure Constants =
struct
    val DEBUG = false
    val MODEKSEMPLER = Config.MODEKSEMPLER
    val (TEST, BESVARELSE) =
        case rev $ CommandLine.arguments () of
          a :: b :: _ => (b, a)
        | _ => raise Fail "Too few arguments"
    val FILER = TEST ^ "/filer"
    val INDRYK = Config.INDRYK
    val KOLONNER = Config.KOLONNER
end

fun maybeUse f = if OS.FileSys.access (f, nil) then use f else ()
;List.app use ["Produkt.sml",
               "Registrer.sml",
               "Vis.sig", "Vis.sml",
               "Gen.sig", "Gen.sml",
               "ProgressBar.sig", "ProgressBar.sml",
               "Test.sig", "Test.sml",
               "Egenskaber.sig",
               "TestDSL.sig", "TestDSL.sml"];

fun stub _ = raise Test.Tom
;use (Constants.TEST ^ "/stub.sml");

(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)
;println $ "~~~~~ Evaluerer '" ^ Constants.BESVARELSE ^ "'...";
;use Constants.BESVARELSE;
(* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - *)

;use (Constants.TEST ^ "/typer.sml");

open Registrer Vis Produkt infix 4 --> & (* $ har 3 *)
;maybeUse (Constants.TEST ^ "/visere.sml");
;use (Constants.TEST ^ "/funktioner.sml");

;OS.FileSys.mkDir (Constants.TEST ^ "/filer") handle OS.SysErr _ => ();
;loadPath := (Constants.TEST ^ "/filer") :: !loadPath;
;maybeUse (Constants.TEST ^ "/vejl.sml");

(* datatype ('a, 'b) product = & of 'a * 'b *)
open TestDSL
infixr 0 slut er
infix 1 afproev note hvor og
infix 2 ? indeholder
infix 3 ::: ==> ~~> !!! & >>
infix 4 eller
;maybeUse (Constants.TEST ^ "/ekstra.sml");
open Gen
;use (Constants.TEST ^ "/test.sml");

;quit();
