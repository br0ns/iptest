structure Registrer =
struct open Produkt infix &
local
  fun t2 f (x & y) = f x y
  fun t3 f (x & y) = t2 f x y
  fun t4 f (x & y) = t3 f x y
  fun t5 f (x & y) = t4 f x y
  fun t6 f (x & y) = t5 f x y
  fun v2 (a & b) (x & y) = Layout.fsep [a x, b y]
  fun v3 (a & b) (x & y) = Layout.fsep [v2 a x, b y]
  fun v4 (a & b) (x & y) = Layout.fsep [v3 a x, b y]
  fun v5 (a & b) (x & y) = Layout.fsep [v4 a x, b y]
  fun v6 (a & b) (x & y) = Layout.fsep [v5 a x, b y]
in
fun funktion n f (va, vr) = ((n, f, va, vr), nil)
                            (* Følgende giver en anelse bedre fejlbeskeder hvis
                             * testsættet ikke indeholder typeangivelser
                             *)
                            before (fn x => (va x ; vr $ f x))
val funktion1 = funktion
fun funktion2 n f (va, vr) = funktion n (t2 f) (v2 va, vr)
fun funktion3 n f (va, vr) = funktion n (t3 f) (v3 va, vr)
fun funktion4 n f (va, vr) = funktion n (t4 f) (v4 va, vr)
fun funktion5 n f (va, vr) = funktion n (t5 f) (v5 va, vr)
fun funktion6 n f (va, vr) = funktion n (t6 f) (v6 va, vr)
end

fun --> (a, b) = (a, b)

end
