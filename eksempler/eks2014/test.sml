saet "IP-eksamen 2014" er

opgave "1" "Rektangler"
  afproev corners
  ? Rectangle ((0, 0), (10, 10))
        ::: permutation [ (0, 0), (0, 10), (10, 0), (10, 10) ]
  ? Rectangle ((10, 20), (30, 40))
        ::: permutation [ (10, 20), (10, 40), (30, 20), (30, 40) ]
  ? Rectangle ((~20, ~45), (~5, ~17))
        ::: permutation [ (~20, ~45), (~20, ~17), (~5, ~45), (~5, ~17) ]
  ? Rectangle ((~10, ~3), (34, 64))
        ::: permutation [ (~10, ~3), (~10, 64), (34, ~3), (34, 64) ]

  afproev inside
  ? Rectangle ((0, 0), (10, 10))      & (5, 5)     ==> true
  ? Rectangle ((0, 0), (10, 10))      & (~2, 5)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (12, 5)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (5, ~2)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (5, 12)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (~2, 12)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (5, 0)     ==> true
  ? Rectangle ((10, 20), (30, 40))    & (13, 32)   ==> true
  ? Rectangle ((10, 20), (30, 40))    & (9, 32)    ==> false
  ? Rectangle ((~10, ~3), (34, 64))   & (~5, ~2)   ==> true
  ? Rectangle ((~10, ~3), (34, 64))   & (~5, 12)   ==> true
  ? Rectangle ((~10, ~3), (34, 64))   & (23, ~2)   ==> true
  ? Rectangle ((~10, ~3), (34, 64))   & (23, 12)   ==> true
  ? Rectangle ((~10, ~3), (34, 64))   & (~5, 75)   ==> false
  ? Rectangle ((~20, ~45), (~5, ~17)) & (~7, ~32)  ==> true
  ? Rectangle ((~20, ~45), (~5, ~17)) & (7, 32)    ==> false
  ? Rectangle ((~20, ~45), (~5, ~17)) & (~32, ~57) ==> false
  ? Rectangle ((0, 0), (10, 10))      & (~5, ~5)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (~5, 0)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (~5, 5)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (~5, 10)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (~5, 15)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (0, ~5)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (0, 0)    ==> true
  ? Rectangle ((0, 0), (10, 10))      & (0, 5)    ==> true
  ? Rectangle ((0, 0), (10, 10))      & (0, 10)   ==> true
  ? Rectangle ((0, 0), (10, 10))      & (0, 15)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (5, ~5)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (5, 0)    ==> true
  ? Rectangle ((0, 0), (10, 10))      & (5, 5)    ==> true
  ? Rectangle ((0, 0), (10, 10))      & (5, 10)   ==> true
  ? Rectangle ((0, 0), (10, 10))      & (5, 15)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (10, ~5)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (10, 0)    ==> true
  ? Rectangle ((0, 0), (10, 10))      & (10, 5)    ==> true
  ? Rectangle ((0, 0), (10, 10))      & (10, 10)   ==> true
  ? Rectangle ((0, 0), (10, 10))      & (10, 15)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (15, ~5)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (15, 0)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (15, 5)    ==> false
  ? Rectangle ((0, 0), (10, 10))      & (15, 10)   ==> false
  ? Rectangle ((0, 0), (10, 10))      & (15, 15)   ==> false

  afproev collisionRect
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((0, 0), (10, 10))   ==> true
  ? Rectangle ((10, 20), (30, 40)) & Rectangle ((10, 20), (30, 40)) ==> true
  ? Rectangle ((~7, ~4), (~5, ~1)) & Rectangle ((~7, ~4), (~5, ~1)) ==> true
  ? Rectangle ((~10, ~3), (34, 6)) & Rectangle ((~10, ~3), (34, 6)) ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((10, 0), (20, 20))  ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((10, 0), (20, 20))  ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((0, 10), (10, 20))  ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((2, 2), (8, 8))     ==> true
  ? Rectangle ((2, 2), (8, 8))     & Rectangle ((0, 0), (10, 10))   ==> true
  ? Rectangle ((0, 5), (15, 10))   & Rectangle ((5, 0), (10, 15))   ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((10, 10), (20, 20)) ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((~7, ~4), (~5, ~1)) ==> false
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((0, 12), (10, 14))  ==> false
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((13, 17), (23, 27)) ==> false
  ? Rectangle ((~10, ~3), (34, 6)) & Rectangle ((0, 0), (10, 10))   ==> true
  ? Rectangle ((~10, ~3), (34, 6)) & Rectangle ((~7, ~4), (~5, ~1)) ==> true
  ? Rectangle ((~4, ~3), (5, 6))   & Rectangle ((~7, ~5), (~5, ~4)) ==> false
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((~2, 8), (2, 12))   ==> true
  ? Rectangle ((0, 0), (10, 10))   & Rectangle ((8, ~2), (12, 2))   ==> true
  ? Rectangle ((~2, 8), (2, 12))   & Rectangle ((0, 0), (10, 10))   ==> true
  ? Rectangle ((8, ~2), (12, 2))   & Rectangle ((0, 0), (10, 10))   ==> true
slut

opgave "2" "Fibber"
  note "Funktionen skal være halerekursiv"

  afproev fibber'
  ?  0 ==> 0
  ?  1 ==> 1
  ?  2 ==> 2
  ?  3 ==> 2
  ?  4 ==> 3
  ?  5 ==> 5
  ?  6 ==> 7
  ?  7 ==> 10
  ?  8 ==> 15
  ?  9 ==> 22
  ? 10 ==> 32
  ? 11 ==> 47
  ? 12 ==> 69
  ? 13 ==> 101
  ? 14 ==> 148
  ? 15 ==> 217
  ? 16 ==> 318
  ? 17 ==> 466
  ? 18 ==> 683
  ? 19 ==> 1001
  ? 20 ==> 1467
  ? 21 ==> 2150
  ? 22 ==> 3151
  ? 23 ==> 4618
  ? 24 ==> 6768
  ? 25 ==> 9919
  ? 26 ==> 14537
  ? 27 ==> 21305
  ? 28 ==> 31224
  ? 29 ==> 45761
  ? 30 ==> 67066
  ? 31 ==> 98290
  ? 32 ==> 144051
  ? 33 ==> 211117
  ? 34 ==> 309407
  ? 35 ==> 453458
  ? 36 ==> 664575
  ? 37 ==> 973982
  ? 38 ==> 1427440
  ? 39 ==> 2092015
  ? 40 ==> 3065997
  ? 41 ==> 4493437
  ? 42 ==> 6585452
  ? 43 ==> 9651449
  ? 44 ==> 14144886
  ? 45 ==> 20730338
  ? 46 ==> 30381787
  ? 47 ==> 44526673
  ? 48 ==> 65257011
  ? 49 ==> 95638798
  ? 50 ==> 140165471
  ? 10000 ::: kaster Overflow
slut

opgave "3" "Højereordensfunktioner"
  note "Kan max opnå halvdelen af pointene hvis de bruger eksplicit rekursion"

  afproev minMax
  ? [1, 2, 3, 4, 5]                ==> (1, 5)
  ? [5, 4, 3, 2, 1]                ==> (1, 5)
  ? [1, 1, 1, 1, 1]                ==> (1, 1)
  ? [1]                            ==> (1, 1)
  ? [1, 2]                         ==> (1, 2)
  ? [2, 1]                         ==> (1, 2)
  ? []                             ::: kaster Empty
  ? [4, 2, 8, ~2, 7, ~1, 4, ~7, 6] ==> (~7, 8)
  ? [~2, 20, 16, 4, ~18, 8]        ==> (~18, 20)
  ? [~3, ~2, ~1]                   ==> (~3, ~1)

  afproev dotProduct
  ? ([0.4, 0.3], [0.1, 0.2])                ~~> 0.1
  ? ([0.4, 0.3], [0.1, 0.2, 0.7])           ~~> 0.1
  ? ([0.1, 0.2, 0.7], [0.4, 0.3])           ~~> 0.1
  ? ([], [])                                ~~> 0.0
  ? ([1.0, 2.0], [])                        ~~> 0.0
  ? ([], [1.0, 2.0])                        ~~> 0.0
  ? ([1.0, 2.0, 3.0], [4.0, 5.0, 6.0])      ~~> 32.0
  ? ([1.0, 2.0], [3.0, 4.0, 5.0, 6.0, 7.0]) ~~> 11.0
  ? ([3.0, 4.0, 5.0, 6.0, 7.0], [1.0, 2.0]) ~~> 11.0
  ? ([~10.0, 1.0, ~6.0, 5.0, 0.0, ~7.0, 1.0],
     [~8.0, ~2.0, ~4.0, 0.0, ~6.0, 7.0, 7.0]) ~~> 60.0

  afproev fromTo
  ? (2, 5)   ==> [2, 3, 4, 5]
  ? (5, 2)   ==> [5, 4, 3, 2]
  ? (7, 7)   ==> [7]
  ? (~2, ~2) ==> [~2]
  ? (0, 0)   ==> [0]
  ? (0, 3)   ==> [0, 1, 2, 3]
  ? (3, 0)   ==> [3, 2, 1, 0]
  ? (~3, 3)  ==> [~3, ~2, ~1, 0, 1, 2, 3]
  ? (3, ~3)  ==> [3, 2, 1, 0, ~1, ~2, ~3]
  ? (~2, ~7) ==> [~2, ~3, ~4, ~5, ~6, ~7]
  ? (~7, ~2) ==> [~7, ~6, ~5, ~4, ~3, ~2]
  ? ( 1000000,  1000004) ==> [ 1000000,  1000001,  1000002,  1000003,  1000004]
  ? (~1000000, ~1000004) ==> [~1000000, ~1000001, ~1000002, ~1000003, ~1000004]
slut

opgave "4" "stringTree"
  note "d) ^^ skal være infix og have præcedens 6"
  note "g) Det er ikke tilladt at fladgøre træet."
  note "h) Må ikke løses vha. toString."

  med datafilerne "printTree"

  afproev fromString
  ? "abc"                     ==> String (3, "abc")
  ? ""                        ==> String (0, "")
  ? "a"                       ==> String (1, "a")
  ? "kattemis"                ==> String (8, "kattemis")
  ? "Dette er en tekststreng" ==> String (23, "Dette er en tekststreng")
  ? "Dette 3r 3n tekst$treng" ==> String (23, "Dette 3r 3n tekst$treng")


  afproev toString
  ? String (0, "")                         ==> ""
  ? String (3, "abc")                      ==> "abc"
  ? String (23, "Dette er en tekststreng") ==> "Dette er en tekststreng"
  ? Concat (0, String (0, ""), 0, String (0, ""))    ==> ""
  ? Concat (0, String (0, ""), 3, String (3, "abc")) ==> "abc"
  ? Concat (3, String (3, "abc"), 0, String (0, "")) ==> "abc"
  ? Concat (3, String (3, "abc"), 3, String (3, "def")) ==> "abcdef"
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
                                                    ==> "abcdefgh"

  afproev size
  ? String (0, "")                         ==> 0
  ? String (3, "abc")                      ==> 3
  ? String (23, "Dette er en tekststreng") ==> 23
  ? Concat (0, String (0, ""), 0, String (0, ""))        ==> 0
  ? Concat (0, String (0, ""), 3, String (3, "abc"))     ==> 3
  ? Concat (3, String (3, "abc"), 0, String (0, ""))     ==> 3
  ? Concat (3, String (3, "abc"), 3, String (3, "def"))  ==> 6
  ? Concat (3, String (3, "abc"), 4, String (4, "defg")) ==> 7
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
                                                    ==> 8

  afproev ^^
  ? (String (3, "abc"), String (2, "de")) ==>
      Concat (3, String (3, "abc"), 2, String (2, "de"))
  ? (String (0, ""), String (0, "")) ==>
      Concat (0, String (0, ""), 0, String (0, ""))
  ? (String (5, "abcde"), String (23, "Dette er en tekststreng")) ==>
      Concat (5,  String (5, "abcde"),
              23, String (23, "Dette er en tekststreng"))
  ? (String (3, "foo"), Concat (2, String (2, "ba"), 4, String (4, "rbaz"))) ==>
     Concat (3, String (3, "foo"),
             6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")))
  ? (Concat (2, String (2, "ba"), 4, String (4, "rbaz")), String (3, "foo")) ==>
     Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
             3, String (3, "foo"))
  ? (Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                        2, Concat (1, String (1, "c"), 1, String (1, "d"))),
             4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                        2, Concat (1, String (1, "g"), 1, String (1, "h")))),
     Concat (4, Concat (2, Concat (1, String (1, "i"), 1, String (1, "j")),
                        2, Concat (1, String (1, "k"), 1, String (1, "l"))),
             5, Concat (2, Concat (1, String (1, "m"), 1, String (1, "n")),
                        3, Concat (1, String (1, "o"), 2, String (2, "pq")))))
        ==>
    Concat (8,
     Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                        2, Concat (1, String (1, "c"), 1, String (1, "d"))),
             4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                        2, Concat (1, String (1, "g"), 1, String (1, "h")))),
            9,
     Concat (4, Concat (2, Concat (1, String (1, "i"), 1, String (1, "j")),
                        2, Concat (1, String (1, "k"), 1, String (1, "l"))),
             5, Concat (2, Concat (1, String (1, "m"), 1, String (1, "n")),
                        3, Concat (1, String (1, "o"), 2, String (2, "pq")))))

  afproev valid
  ? String (2, "abc")    ==> false
  ? String (3, "abc")    ==> true
  ? String (2, "de")     ==> true
  ? String (23, "Dette er en tekststreng") ==> true
  ? String (20, "Dette er en tekststreng") ==> false
  ? Concat (3, String (3, "abc"), 2, String (2, "de"))  ==> true
  ? Concat (3, String (3, "abc"), 2, String (2, "def")) ==> false
  ? Concat (3, String (3, "abc"), 2, String (3, "de"))  ==> false
  ? Concat (3, String (3, "abc"), 3, String (2, "de"))  ==> false
  ? Concat (3, String (3, "abc"), 3, String (2, "de"))  ==> false
  ? Concat (3, String (3, "abcd"), 2, String (2, "de")) ==> false
  ? Concat (3, String (2, "abc"), 2, String (2, "de"))  ==> false
  ? Concat (2, String (3, "abc"), 2, String (2, "de"))  ==> false
  ? Concat (2, String (2, "abc"), 2, String (2, "de"))  ==> false
  ? Concat (2, String (2, "abc"), 3, String (3, "de"))  ==> false
  ? Concat (0, String (0, ""), 0, String (0, ""))       ==> true
  ? Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
            3, String (3, "foo"))                                    ==> true
  ? Concat (3, String (3, "foo"),
            6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")))  ==> true
  ? Concat (3, String (2, "foo"),
            6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")))  ==> false
  ? Concat (3, String (3, "foo"),
            6, Concat (2, String (2, "ba"), 4, String (4, "rba")))   ==> false
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
            ==> true
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "fX")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
            ==> false
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 2, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
            ==> false
  ? Concat (5, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
            ==> false
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            5, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
            ==> false

  afproev sub
  ? Concat (3, String (3, "abc"), 2, String (2, "de")) & 4 ==> #"e"
  ? String (23, "Dette er en tekststreng") & 0  ==> #"D"
  ? String (23, "Dette er en tekststreng") & 22 ==> #"g"
  ? String (23, "Dette er en tekststreng") & 7  ==> #"r"
  ? String (23, "Dette er en tekststreng") & ~3 ::: kaster Subscript
  ? String (23, "Dette er en tekststreng") & 25 ::: kaster Subscript
  ? Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
            3, String (3, "foo")) & 1 ==> #"a"
  ? Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
            3, String (3, "foo")) & 6 ==> #"f"
  ? String (0, "") & 0 ::: kaster Subscript
  ? String (0, "") & 1 ::: kaster Subscript
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h")))) & 6
                            ==> #"g"
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h")))) & 1
                            ==> #"b"
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h")))) & 8
                            ::: kaster Subscript
  ? Concat (3, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       1, Concat (0, String (0, ""), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h")))) & 6
                            ==> #"h"
  afproev subTree
  ? String (23, "Dette er en tekststreng") & (9, 8)
              ==> String (8, "en tekst")
  ? String (23, "Dette er en tekststreng") & (9, 0)
              ==> String (0, "")
  ? String (23, "Dette er en tekststreng") & (9, 56)
              ::: kaster Subscript
  ? String (23, "Dette er en tekststreng") & (~2, 5)
              ::: kaster Subscript
  ? String (23, "Dette er en tekststreng") & (9, ~2)
              ::: kaster Subscript
  ? Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
            3, String (3, "foo")) & (0, 9)
    ==> Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
                3, String (3, "foo"))
  ? Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
            3, String (3, "foo")) & (1, 3)
    ==> Concat (1, String (1, "a"), 2, String (2, "rb"))
  ? Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
            3, String (3, "foo")) & (3, 5)
    ==> Concat (3, String (3, "baz"), 2, String (2, "fo"))
  ? Concat (3, String (3, "abc"), 2, String (2, "de")) & (2, 2)
              ==> Concat (1, String (1, "c"), 1, String (1, "d"))
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
        & (2, 3)
      ==> Concat (2, Concat (1, String (1, "c"), 1, String (1, "d")),
                  1, String (1, "e"))
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
        & (1, 6)
     ==> Concat (3, Concat (1, String (1, "b"),
                            2, Concat (1, String (1, "c"), 1, String (1, "d"))),
                 3, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                            1, String (1, "g")))
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
        & (9,0) ::: kaster Subscript
  ? Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
        & (~1,0) ::: kaster Subscript
  ? Concat (3, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       1, Concat (0, String (0, ""), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h"))))
        & (1, 5) ::: blandt [
         Concat (2, Concat (1, String (1, "b"),
                            1, Concat (0, String (0, ""), 1, String (1, "d"))),
                 3, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                            1, String (1, "g"))),
         Concat (2, Concat (1, String (1, "b"),
                            1, String (1, "d")),
                 3, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                            1, String (1, "g")))
        ]

  afproev printTreeFile
  ? (fil "empty-out.txt", String (0, ""))
      ::: giverFilen "empty-out.txt" matchende "empty.txt"
  ? (fil "simple-out.txt", String (23, "Dette er en tekststreng"))
      ::: giverFilen "simple-out.txt" matchende "simple.txt"
  ? (fil "concat-out.txt", Concat (3, String (3, "abc"), 2, String (2, "de")))
      ::: giverFilen "concat-out.txt" matchende "concat.txt"
  ? (fil "concat-mixed1-out.txt",
        Concat (6, Concat (2, String (2, "ba"), 4, String (4, "rbaz")),
                3, String (3, "foo")))
      ::: giverFilen "concat-mixed1-out.txt" matchende "concat-mixed1.txt"
  ? (fil "concat-mixed2-out.txt",
        Concat (3, String (3, "foo"),
                6, Concat (2, String (2, "ba"), 4, String (4, "rbaz"))))
      ::: giverFilen "concat-mixed2-out.txt" matchende "concat-mixed2.txt"
  ? (fil "concat-complex-out.txt",
    Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (1, "c"), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h")))))
      ::: giverFilen "concat-complex-out.txt" matchende "concat-complex.txt"
  ? (fil "concat-complex2-out.txt",
    Concat (4, Concat (2, Concat (1, String (1, "a"), 1, String (1, "b")),
                       2, Concat (1, String (0, ""), 1, String (1, "d"))),
            4, Concat (2, Concat (1, String (1, "e"), 1, String (1, "f")),
                       2, Concat (1, String (1, "g"), 1, String (1, "h")))))
      ::: giverFilen "concat-complex2-out.txt" matchende "concat-complex2.txt"
slut

opgave "5" "Tekstsøgning"
  hvor "tekst" indeholder
    "Dette er en ganske almindelig tekst.\n" ^
    "Den er på flere linjer.\n" ^
    "Sikke nogle fine tests man kan få ud af den."

  hvor "tom" indeholder ""

  hvor "mellemrum" indeholder
    "Linje et\n" ^
    " Linje to"

  hvor "tomlinje" indeholder
    "Linje et\n\n" ^
    " Linje tre"

  afproev search
  ? "" & "foo" ==> SOME 0
  ? "fo" & "foo" ==> SOME 0
  ? "en" & "I morgen er verden vor" ==> SOME 6
  ? "en vor" & "I morgen er verden vor" ==> SOME 16
  ? "hello world" & "I morgen er verden vor" ==> NONE
  ? "" & "abcdefgh" ==> SOME 0
  ? "test" & "Dette er en test." ==> SOME 12
  ? "Test" & "Dette er en test." ==> NONE
  ? "test" & "Dette er en Test." ==> NONE
  ? "test" & "Der er flere linjer\ni denne test." ==> SOME 28
  ? "test" & "Ordet test forekommer flere gange i denne test. (test)" ==> SOME 6
  ? "test" & "test er første ord i denne test." ==> SOME 0
  ? "a" & "bcdefghijklmn" ==> NONE
  ? "tom" & "" ==> NONE
  ? "" & "" ==> SOME 0
  ? "ab" & "aab" ==> SOME 1
  ? "aaab" & "aaaab" ==> SOME 1
  ? "abcd" & "axbyczd_abcd" ==> SOME 8

  afproev searchFile
  ? "" & fil "tekst" ==> SOME (1, 0)
  ? "Dette" & fil "tekst" ==> SOME (1, 0)
  ? "Den" & fil "tekst" ==> SOME (2, 0)
  ? "en" & fil "tekst" ==> SOME (1, 9)
  ? "fine" & fil "tekst" ==> SOME (3, 12)
  ? "test" & fil "tekst" ==> SOME (3, 17)
  ? "Test" & fil "tekst" ==> NONE
  ? "sikke" & fil "tekst" ==> NONE
  ? "ukendt" & fil "tekst" ==> NONE
  ? "Linje to" & fil "mellemrum" ==> SOME (2, 1)
  ? "Linje tre" & fil "tomlinje" ==> SOME (3, 1)
  ? "" & fil "tom" ::: blandt [SOME (1, 0), NONE]
slut

opgave "6" "Grupperinger"
  afproev insertG
  ? (0, 42, [[1,2],[3,4],[5,6]]) ==> [[42,1,2],[3,4],[5,6]]
  ? (1, 42, [[1,2],[3,4],[5,6]]) ==> [[1,2],[42,3,4],[5,6]]
  ? (2, 42, [[1,2],[3,4],[5,6]]) ==> [[1,2],[3,4],[42,5,6]]
  ? (3, 42, [[1,2],[3,4],[5,6]]) ==> [[1,2],[3,4],[5,6],[42]]
  ? (4, 42, [[1,2],[3,4],[5,6]]) ==> [[1,2],[3,4],[5,6],[],[42]]
  ? (7, 42, [[1,2],[3,4],[5,6]]) ==> [[1,2],[3,4],[5,6],[],[],[],[],[42]]
  ? (~4, 42, [[1,2],[3,4],[5,6]]) ::: kaster Domain
  ? (0, 42, []) ==> [[42]]
  ? (0, 42, [[1]]) ==> [[42,1]]
  ? (10, 42, []) ==> [[],[],[],[],[],[],[],[],[],[],[42]]

  afproev group
  ? (fn x => x mod 3) & [0,1,2,3,4,5,6,7]
          ::: elementpermutation [[6,3,0],[7,4,1],[5,2]]
  ? (fn x => x mod 5) & [5,9,4,0]
          ::: elementpermutation [[0,5], [], [], [], [4,9]]
  ? (fn x => x mod 5) & [4]
          ::: elementpermutation [[], [], [], [], [4]]
  ? (fn x => x mod 3) & [0,1]
          ::: elementpermutation [[0],[1]]
  ? (fn x => x) & [0,1,2,3,4,~5,6,7] ::: kaster Domain
  ? (fn x => x mod 4) & [] ==> []
  ? (fn x => x div 10) & [1, 2, 22, 66, 70, 3, 10, 12, 4, 14]
          ::: elementpermutation [[1,2,3,4],[10,12,14],[22],[],[],[],[66],[70]]
slut

opgave "7" "Kø-struktur"
  note "Strukturen skal være uigennemsigtig"

  afproev queueFuncs
  ? [Dequeue] ::: kaster Empty
  ? [Enqueue 5, Dequeue] ==> [5]
  ? [Enqueue 5, Enqueue 6, Dequeue, Dequeue] ==> [5, 6]
  ? [Enqueue 5, Dequeue, Enqueue 6, Dequeue] ==> [5, 6]
  ? [Enqueue 5, Enqueue 6, Dequeue, Enqueue 7, Dequeue, Dequeue] ==> [5,6,7]
  ? [Enqueue 5, Dequeue, Dequeue] ::: kaster Empty
  ? [Enqueue 5, Enqueue 6, Dequeue, Enqueue 7, Dequeue, Dequeue, Dequeue]
                                                      ::: kaster Empty
slut

heltslut
