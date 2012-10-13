local open Layout infix ^^ ++ \ & \\ && in
fun pred _ = txt "<funktion>"

fun btree _ Blad = txt "Blad"
  | btree pp (Gren (t1, x, t2)) =
    txt "Gren (" & btree pp t1 && txt ","
        & pp x && txt "," & btree pp t2 && txt ")"

fun udvidelse pp =
 fn  %  => txt "%"
   | << => txt "<<"
   | >> => txt ">>"
   | == x => txt "==" & pp x
end
