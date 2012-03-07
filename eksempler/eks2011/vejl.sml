structure Vejl =
struct
local
local fun kth (y :: _) 1 = y
        | kth (_ :: yr) n = kth yr (n-1)
in fun f _ nil = nil
     | f xr (n :: nr) = (kth xr n) :: f xr nr
end

fun g xs nil = xs
  | g xs (f :: fs) =
    let fun loop nil = nil
          | loop (y :: ys) = if f y then y :: loop ys else loop ys
    in g (loop xs) fs
    end
fun h [x : real] = (x,x)
  | h (x :: xr) = let val (frst,sdst) = h xr
                  in if x < frst then (x,sdst)
                     else if x > sdst then (frst,x) else (frst,sdst)
                  end
in
val f = fn (a & b) => f a b
val g = fn (a & b) => g a b
val h = h
end

fun display (n & i) = StringCvt.padLeft #" " n $ Int.toString i
end
