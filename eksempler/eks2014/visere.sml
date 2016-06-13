local open Layout infix ^^ ++ \ & \\ && in
  val point = par (int, int)

  fun rect (Rectangle t) =
      txt "Rectangle" & par (point, point) t

  fun stringTree (String t) =
      txt "String" & par (int, string) t
    | stringTree (Concat t) =
      txt "Concat" & tupel4 (int, stringTree, int, stringTree) t

  fun outstream _ = txt "<outstream>"
  fun func _ = txt "<fn>"

  fun queueop (Enqueue n) = txt "Enqueue" & int n
    | queueop Dequeue = txt "Dequeue"
end
