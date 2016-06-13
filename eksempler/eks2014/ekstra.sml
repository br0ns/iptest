; val _ = load "Listsort" ;
local
  structure LS = Listsort
in
fun elementpermutation x =
    aekvivalens
      "en elementvis permutation af _"
      (fn (x, y) => map (LS.sort Int.compare) x = map (LS.sort Int.compare) y)
      x
end
