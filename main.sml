val use = fn f => use f handle _ => quit ()
;use "MyLib.sml";
;use "Config.sml";

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
;List.app use ["Registrer.sml",
               "Vis.sig", "Vis.sml",
               "Test.sig", "Test.sml",
               "Egenskaber.sig",
               "TestDSL.sig", "TestDSL.sml"];

fun stub _ = raise Test.Tom
;use (Constants.TEST ^ "/stub.sml");

;use Constants.BESVARELSE;

;use (Constants.TEST ^ "/typer.sml");
open Registrer Vis infix 4 -->
;maybeUse (Constants.TEST ^ "/visere.sml");
;use (Constants.TEST ^ "/funktioner.sml");

;loadPath := (Constants.TEST ^ "/filer") :: !loadPath;
;maybeUse (Constants.TEST ^ "/vejl.sml");

open TestDSL
infixr 0 slut er
infix 1 afproev note hvor og
infix 2 ? indeholder
infix 3 ::: & ==> ~~> !!!
infix 4 eller
;maybeUse (Constants.TEST ^ "/ekstra.sml");
;use (Constants.TEST ^ "/test.sml");

;quit();
