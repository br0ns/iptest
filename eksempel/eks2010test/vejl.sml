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
fun nummerer xs =
    let
      fun loop _ nil = nil
        | loop n (x :: xs) = (n, x) :: loop (n + 1) xs
    in
      loop 0 xs
    end
fun sammenlign ((_, x), (_, y)) = Real.compare (x, y)

fun sortPerm xs =
    (ListPair.unzip o Listsort.sort sammenlign o nummerer) xs
fun foranAlle _ nil = nil
  | foranAlle x (y :: ys) = (x :: y) :: foranAlle x ys

fun klassedelinger nil = [nil]
  | klassedelinger (x :: xs) =
    let
      fun loop nil = nil
        | loop (k :: ks) = ((x :: k) :: ks) ::
                           foranAlle k (loop ks)
      val k = klassedelinger xs
    in
      foranAlle [x] k @ List.concat (map loop k)
    end
end
