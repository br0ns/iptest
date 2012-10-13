structure ProgressBar =
struct
val BACKSPACE = chr 8

type t = string array * string * real

fun del n = print $ implode $ List.tabulate (n, const BACKSPACE)

fun commonprefix s t =
    let
      fun loop cs nil ys = (cs, nil, ys)
        | loop cs xs nil = (cs, xs, nil)
        | loop cs (x :: xs) (y :: ys) =
          if x <> y then
            (cs, x :: xs, y :: ys)
          else
            loop (x :: cs) xs ys
      val (cs, xs, ys) = loop nil (explode s) (explode t)
    in
      (implode $ rev cs, implode xs, implode ys)
    end

fun defindex (arr, i) =
    Int.max (0, Int.min (Array.length arr - 1, i))
fun defsub (arr, i) =
    Array.sub (arr, defindex (arr, i))
fun defupdate (arr, i, v) =
    Array.update (arr, defindex (arr, i), v)
fun toIndex (arr, p) = round (real (Array.length arr) * p)

fun progress (frames, old, current) p =
    let open Array
      val current = current + p
      val i = toIndex (frames, current)
      val new = defsub (frames, i)
      val (prefix, new', old') = commonprefix new old
    in
      del $ size old'
    ; print new'
    ; (frames, new, current)
    end

fun mk frames = (Array.fromList frames, "", 0.0)

fun repeat s n = if n <= 0 then "" else s ^ repeat s (n - 1)

fun alert (pbar as (frames, old, current)) =
    let open Array
      val duration = 0.1
      val start1 = toIndex (frames, current)
      val stop1 = toIndex (frames, current + duration / 3.0)
      val start2 = toIndex (frames, current + 2.0 * duration / 3.0)
      val stop2 = toIndex (frames, current + duration)
      fun loop i stop =
          if i >= stop then ()
          else
            let
              val frame = defsub (frames, i)
              val frame' = String.map
                             (fn c => if Char.isGraph c then #"!" else c)
                             frame
            in
              defupdate (frames, i, frame')
            ; loop (i + 1) stop
            end
    in
      loop start1 stop1
    ; loop start2 stop2
    ; pbar
    end

fun init w =
    let
      val extrapolate = List.concat o map (fn x => [x, x, x, x, x])
      fun loop n =
          if n >= w then
            [repeat "=" n]
          else repeat "=" (n - 1) ^
               (if      n mod 4 = 0 then "-"
                else if n mod 4 = 1 then "\\"
                else if n mod 4 = 2 then "|"
                else                     "/") ::
               loop (n + 1)
    in
      mk $ extrapolate $ loop 0
    end
end
