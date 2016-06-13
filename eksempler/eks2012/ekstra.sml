; val _ = load "Listsort" ;
local
  structure LS = Listsort
in
fun dybpermutation x =
    aekvivalens
      "en dyb permutation af _"
      (fn (x, y) => Set.equal (Set.map (LS.sort Int.compare) $ Set.fromList x)
                              (Set.map (LS.sort Int.compare) $ Set.fromList y))
      x
end

fun checkListeTabel vs =
    praedikat
      "de korrekte værdier for _"
      (fn t => Vejl.checkLT t vs)

fun checkFunTabel vs =
    praedikat
      "de korrekte værdier for _"
      (fn t => Vejl.checkFT t vs)

fun checkIkkeListeTabel vs =
    praedikat
      "har passende NONEs for _"
      (fn t => Vejl.checkNotLT t vs)

fun checkIkkeFunTabel vs =
    praedikat
      "har passende NONEs for _"
      (fn t => Vejl.checkNotFT t vs)
