structure Gen :> Gen =
struct
type 'a t = Random.generator -> 'a

fun run t = t $ Random.newgen ()

fun unit _ = ()

(* No need to go crazy here *)
fun int g = Random.range (~1000, 1000) g
fun real g = Random.random g
fun bool g = Random.random g > 0.5
fun char g =
    let
      val cs = "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
               \abcdefghijklmnopqrstuvwxyz\
               \1234567890\
               \<>,;.:-_'*^~!\"\\@#$%&/{([)]=}?+'`\n "
    in
      String.sub (cs, Random.range (0, size cs) g)
    end
fun order g =
    let
      val r = Random.range(0, 3) g
    in
      if r = 0 then
        LESS
      else if r = 1 then
        EQUAL
      else
        GREATER
    end

fun option t g = if Random.random g > 0.8 then NONE else SOME $ t g

fun list t g =
    let
      val decay = 0.99
      fun loop x =
          if Random.random g > x then nil else t g :: loop (x * x)
    in
      loop decay
    end

fun string g = implode $ list char g

fun par (t1, t2) g = (t1 g, t2 g)

fun triple (t1, t2, t3) g = (t1 g, t2 g, t3 g)

fun tupel4 (t1, t2, t3, t4) g = (t1 g, t2 g, t3 g, t4 g)

fun tupel5 (t1, t2, t3, t4, t5) g = (t1 g, t2 g, t3 g, t4 g, t5 g)
end
