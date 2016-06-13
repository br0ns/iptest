fun fixPPM infile outfile =
    let
      val is = TextIO.openIn infile
      val contents = TextIO.inputAll is
      val _ = TextIO.closeIn is

      fun split (#"2" :: #"5" :: #"5" :: x :: xs) ls =
          (rev (x :: #"5" :: #"5" :: #"2" :: ls), xs)
        | split (x :: xs) ls = split xs (x::ls)
        | split _ _ = raise Domain

      val (header, body) = split (explode contents) []

      val os = TextIO.openOut outfile
      val res = String.map (
        fn c =>
            case ord c of
                10  => chr 11   (* avoid \n's *)
              | 13  => chr 14   (* avoid \r's *)
              | 26  => chr 27   (* avoid subs *)
              | 229 => chr 228  (* avoid things that'll become subs *)
              | 242 => chr 241  (* avoid things that'll become \r's *)
              | 245 => chr 244  (* avoid things that'll become \n's *)
              |  _ => c
        ) (implode body)
    in
        TextIO.output (os, implode header);
        TextIO.output (os, res);
        TextIO.closeOut os
    end
