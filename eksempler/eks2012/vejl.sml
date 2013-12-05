structure Vejl =
struct
    val makeLT = foldl (fn (p,a) => ListeTabel.indsaet a p) ListeTabel.tom
    val makeFT = foldl (fn (p,a) => FunTabel.indsaet a p) FunTabel.tom

    fun checkLT t = List.all (fn (k,v) => ListeTabel.find t k = SOME v)
    fun checkFT t = List.all (fn (k,v) => FunTabel.find t k = SOME v)

    fun checkNotLT t = List.all (fn k => ListeTabel.find t k = NONE)
    fun checkNotFT t = List.all (fn k => FunTabel.find t k = NONE)
end
