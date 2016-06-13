structure Vejl =
struct
  val doQueue =
    rev o #2 o foldl (fn (Enqueue s, (q, r)) => (Queue.enqueue (s, q), r)
                       | (Dequeue, (q, r)) =>
                           let val (s, q') = Queue.dequeue q
                           in (q', s :: r) end)
                     (Queue.empty, [])

  fun printTreeFile (file, st) =
    let val os = TextIO.openOut file
    in printTree (os, st)
     ; TextIO.output (os, "\n") (* our reference files end with a trailing \n *)
        (* also just so happens to catch if they closed the stream themselves *)
     ; TextIO.closeOut os
    end
end
