structure Vis :> Vis =
struct
open Layout
infix ^^ ++ \ & \\ &&

val unit = const $ txt "()"
val char = chr
val string = softtxt
fun option _ NONE = txt "NONE"
  | option f (SOME x) = txt "SOME" & parens (f x)

fun order LESS = txt "LESS"
  | order EQUAL = txt "EQUAL"
  | order GREATER = txt "GREATER"

val commasep = fsep o punctuate comma

fun list f = brackets o commasep o map f
fun par (f, g) (a, b) = parens $ commasep [f a, g b]
fun triple (f, g, h) (a, b, c) = parens $ commasep [f a, g b, h c]
fun tupel4 (f, g, h, i) (a, b, c, d) =
    parens $ commasep [f a, g b, h c, i d]
fun tupel5 (f, g, h, i, j) (a, b, c, d, e) =
    parens $ commasep [f a, g b, h c, i d, j e]

end
