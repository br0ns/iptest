fun lineariser Blad = [%]
  | lineariser (Gren (t1, x, t2)) =
    << :: lineariser t1 @ == x :: lineariser t2 @ [>>]
