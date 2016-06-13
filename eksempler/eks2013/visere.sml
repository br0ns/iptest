local open Layout infix ^^ ++ \ & \\ && in
fun command Turn            = txt "Turn"
  | command (Move n)        = txt "Move" & int n
  | command (Repeat (n,cs)) = txt "Repeat" & par (int, list command) (n, cs)

fun heading North = txt "North"
  | heading South = txt "South"
  | heading East  = txt "East"
  | heading West  = txt "West"

val point = par (int, int)
val drawing = list point
val program = list command
val state = par (point, heading)

end
