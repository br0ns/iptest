structure Vejl =
struct
fun eval p a =
    case p of
      TT => true
    | FF => false
    | AND (p1, p2) => eval p1 a andalso eval p2 a
    | OR (p1, p2) => eval p1 a orelse eval p2 a
    | NOT p => not $ eval p a
    | VAR s => a s
end
