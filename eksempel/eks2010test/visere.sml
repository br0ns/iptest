local open Layout infix ^^ ++ \ & \\ && in
val kalender = vsep o map (par (triple (int, int, int), string))

fun prop p =
    case p of
      VAR s => txt "VAR" & txt s
    | NOT p => txt "NOT" & parens $ prop p
    | AND ps => txt "AND" & par (prop, prop) ps
    | OR ps => txt "OR" & par (prop, prop) ps
    | TT => txt "TT"
    | FF => txt "FF"

val tildeling = const $ txt "[se tildelingsfunktion i 'ekstra.sml']"

fun klist v xs = txt "k-listen svarende til" & list v $ TrivKList.tilListe xs
end
