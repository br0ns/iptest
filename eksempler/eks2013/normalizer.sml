local
  (* drops from head of list as long as predicate says to *)
  fun dropP p []      = []
    | dropP p (x::xs) = if p x then dropP p xs else x::xs

  (* replaces newlines with spaces *)
  fun removeNewlines xs = map (fn #"\n" => #" "
                                | #"\r" => #" "
                                | x     => x) xs

  (* collapses multiple whitespace to a single one *)
  fun collapseWhitespace [] = []
    | collapseWhitespace (c::cs) =
        if Char.isSpace c
        then c :: collapseWhitespace (dropP Char.isSpace cs)
        else c :: collapseWhitespace cs

  (* adds newlines after each tag *)
  fun newLineTags [] = []
    | newLineTags (#">"::xs) = #">" :: #"\n" ::
                                       newLineTags (dropP Char.isSpace xs)
    | newLineTags (x :: xs)  = x :: newLineTags xs
in
(* Forsøger at normalisere en SVG, så den kan sammenlignes mod en referencefil*)
fun normaliserSVG fil =
  let val is = TextIO.openIn fil
      val data = TextIO.inputAll is
      val _ = TextIO.closeIn is

      val data' = (newLineTags o collapseWhitespace o removeNewlines)
                    (dropP Char.isSpace (explode data))

      val os = TextIO.openOut fil
      val _ = TextIO.output (os, implode data')
  in TextIO.closeOut os end
end
