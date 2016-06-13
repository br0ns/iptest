saet "IP-eksamen 2012" er

opgave "1" "Palindromer"
  afproev erPalindrom
  ? ""                                ==> true
  ? "pip"                             ==> true
  ? "ab"                              ==> false
  ? "aA"                              ==> false
  ? "regninger mellem regninger"      ==> true
  ? "p"                               ==> true
  ? "otto"                            ==> true
  ? "abcde.fgh hgf.edcba"             ==> true
  ? "123abccba321"                    ==> true
  ? "Otto"                            ==> false
  ? "kandidat"                        ==> false
  ? "aptapa"                          ==> false
  ? "ni.n"                            ==> false
  ? "a toyota"                        ==> false
  ? "A Toyota"                        ==> false
  ? "A man, a plan, a canal: Panama!" ==> false
  ? "Mr. Owl Ate My Metal Worm"       ==> false
  ? "Zeus was deified, saw Suez."     ==> false

  afproev erUdvidetPalindrom
  ? ""                                ==> true
  ? "ab"                              ==> false
  ? "aA"                              ==> true
  ? "pip"                             ==> true
  ? "regninger mellem regninger"      ==> true
  ? "p"                               ==> true
  ? "otto"                            ==> true
  ? "abcde.fgh hgf.edcba"             ==> true
  ? "Otto"                            ==> true
  ? "123abccba321"                    ==> true
  ? "kandidat"                        ==> false
  ? "aptapa"                          ==> false
  ? "ni.n"                            ==> true
  ? "a toyota"                        ==> true
  ? "A Toyota"                        ==> true
  ? "A man, a plan, a canal: Panama!" ==> true
  ? "Mr. Owl Ate My Metal Worm"       ==> true
  ? "Zeus was deified, saw Suez."     ==> true
  ? "On tub, Edward imitated a cadet; a timid raw debut, no?" ==> true
  ? "On tub, Edward imitated a cedet; a timid raw debut, no?" ==> false
  ? "bo\nb"                           ==> true
  ? "bo\tb"                           ==> true
  ? "mellem1"                         ==> false
slut


opgave "2" "Ruter"
  afproev korrekt
  ? Stop                       ==> true
  ? Drej (0, Stop)             ==> true
  ? Frem (0, Stop)             ==> true
  ? Frem (10, Stop)            ==> true
  ? Drej (10, Stop)            ==> true
  ? Drej (200, Stop)           ==> true
  ? Frem (10, Frem (20, Stop)) ==> true
  ? Frem (10, Drej (20, Stop)) ==> true
  ? Drej (10, Frem (20, Stop)) ==> true
  ? Drej (10, Drej (20, Stop)) ==> true
  ? Frem (10, Drej (90, Frem(40, Frem(50, Drej(40, Frem(5, Stop)))))) ==> true
  ? Frem (~1, Stop)            ==> false
  ? Frem (~40, Stop)           ==> false
  ? Frem (10, Frem (20, Frem (~10, Frem (30, Stop)))) ==> false
  ? Frem (10, Drej (90, Frem(40, Frem(~10, Drej(40, Frem(5, Stop)))))) ==> false

  afproev laengde
  ? Stop                       ==> 0
  ? Drej (0, Stop)             ==> 0
  ? Frem (0, Stop)             ==> 0
  ? Frem (10, Stop)            ==> 10
  ? Drej (10, Stop)            ==> 0
  ? Frem (10, Frem (20, Stop)) ==> 30
  ? Frem (10, Drej (20, Stop)) ==> 10
  ? Drej (10, Frem (20, Stop)) ==> 20
  ? Drej (10, Drej (20, Stop)) ==> 0
  ? Frem (10, Drej (90, Frem(40, Frem(50, Drej(40, Frem(5, Stop)))))) ==> 105

  afproev erNormaliseret
  ? Stop                       ==> true
  ? Drej (0, Stop)             ==> false
  ? Drej (270, Stop)           ==> false
  ? Drej (180, Stop)          ==> true
  ? Drej (~180, Stop)          ==> false
  ? Drej (~270, Stop)          ==> false
  ? Frem (0, Stop)             ==> false
  ? Frem (10, Stop)            ==> true
  ? Drej (10, Stop)            ==> true
  ? Frem (10, Frem (20, Stop)) ==> false
  ? Frem (10, Drej (20, Stop)) ==> true
  ? Drej (10, Frem (20, Stop)) ==> true
  ? Drej (10, Drej (20, Stop)) ==> false
  ? Frem (10, Drej (90, Frem(40, Frem(50, Drej(40, Frem(5, Stop)))))) ==> false
  ? Frem (10, Drej (90, Frem(90, Drej(40, Frem(5, Stop))))) ==> true
  ? Frem (10, Drej (90, Frem(90, Drej(0, Frem(5, Stop))))) ==> false
  ? Frem (10, Drej (90, Frem(90, Drej(270, Frem(5, Stop))))) ==> false
  ? Frem (10, Drej (90, Frem(90, Drej(180, Frem(5, Stop))))) ==> true
  ? Frem (10, Drej (90, Frem(90, Drej(~180, Frem(5, Stop))))) ==> false
  ? Frem (10, Drej (90, Frem(90, Drej(~270, Frem(5, Stop))))) ==> false
  ? Frem (10, Drej (90, Frem(0, Drej(40, Frem(5, Stop))))) ==> false
  ? Frem (10, Drej (90, Drej(90, Drej(40, Frem(5, Stop))))) ==> false
  ? Frem (10, Drej (90, Frem(30, Frem(90, Drej(40, Frem(5, Stop)))))) ==> false

  afproev normaliserRute
  ? Stop                             ==> Stop
  ? Frem(0, Stop)                    ==> Stop
  ? Drej(0, Stop)                    ==> Stop
  ? Drej(~240, Stop)                 ==> Drej(120, Stop)
  ? Drej(560, Stop)                  ==> Drej(~160, Stop)
  ? Drej(~180, Stop)                 ==> Drej(180, Stop)
  ? Drej(~179, Stop)                 ==> Drej(~179, Stop)
  ? Drej(180, Stop)                  ==> Drej(180, Stop)
  ? Drej(181, Stop)                  ==> Drej(~179, Stop)
  ? Frem(56, Stop)                   ==> Frem(56, Stop)
  ? Frem(181, Stop)                  ==> Frem(181, Stop)
  ? Frem(12, Frem(23, Stop))         ==> Frem(35, Stop)
  ? Frem(0, Drej(0, Stop))           ==> Stop
  ? Drej(0, Frem(0, Stop))           ==> Stop
  ? Frem(0, Frem(0, Frem(0, Stop)))  ==> Stop
  ? Frem(0, Frem(12, Frem(0, Stop))) ==> Frem(12, Stop)
  ? Frem(90, Frem (90, Frem (90, Stop))) ==> Frem (270, Stop)
  ? Drej(45, Frem (0, Drej (~45, Stop))) ==> Stop
  ? Drej (45, Frem (0, Drej (135, Frem (0, Drej (180, Stop)))))
                                     ==> Stop
  ? Frem (10, Frem (0, Frem (0, Drej (90, Frem (0, Frem (0, Frem (0, Drej (90,
    Frem (0, Frem (0, Drej (~90, Drej (180, Frem (0, Drej (~90, Drej (180,
    Frem (10, Stop))))))))))))))))   ==> Frem (20, Stop)
  ? Frem(12, Frem(23, Drej(21, Frem(34, Drej(45, Drej(~468, Frem(23, Stop)))))))
                                     ==> Frem(35, Drej(21, Frem(34, Drej(~63,
                                         Frem(23, Stop)))))
slut

opgave "3" "Kvadratfrie tal"
  note "Køretiden må ikke overskride et sekund i a) og c)"
  note "b) skal checkes manuelt"

  afproev kvadratfrit
  ? 1         ==> true
  ? 2         ==> true
  ? 3         ==> true
  ? 4         ==> false
  ? 5         ==> true
  ? 6         ==> true
  ? 7         ==> true
  ? 8         ==> false
  ? 9         ==> false
  ? 10        ==> true
  ? 11        ==> true
  ? 12        ==> false
  ? 13        ==> true
  ? 14        ==> true
  ? 15        ==> true
  ? 16        ==> false
  ? 17        ==> true
  ? 18        ==> false
  ? 19        ==> true
  ? 20        ==> false
  ? 1365      ==> true
  ? 17745     ==> false
  ? 33301     ==> true
  ? 423389    ==> true
  ? 4657634   ==> true
  ? 60052873  ==> true
  ? 589230217 ==> true
  ? 589230225 ==> false
  ? 687742919 ==> true
  ? 847992755 ==> false
  ? 999999937 ==> true
  ? 0     ::: kaster Domain
  ? ~4    ::: kaster Domain

  afproev maksKvadratfrit
  ? 1         ==> 1
  ? 2         ==> 2
  ? 3         ==> 3
  ? 4         ==> 2
  ? 5         ==> 5
  ? 6         ==> 6
  ? 7         ==> 7
  ? 8         ==> 2
  ? 9         ==> 3
  ? 10        ==> 10
  ? 11        ==> 11
  ? 12        ==> 6
  ? 13        ==> 13
  ? 14        ==> 14
  ? 15        ==> 15
  ? 16        ==> 2
  ? 17        ==> 17
  ? 18        ==> 6
  ? 19        ==> 19
  ? 20        ==> 10
  ? 1365      ==> 1365
  ? 17745     ==> 1365
  ? 33301     ==> 33301
  ? 423389    ==> 423389
  ? 4657634   ==> 4657634
  ? 60052873  ==> 60052873
  ? 589230217 ==> 589230217
  ? 589230225 ==> 39282015
  ? 687742919 ==> 687742919
  ? 847992755 ==> 362545
  ? 999999937 ==> 999999937
  ? 0     ::: kaster Domain
  ? ~4    ::: kaster Domain
slut

opgave "4" "Permutationer"
  note "Brug af real i c) er ikke tilladt."
  afproev erPermutationAf
  ? ([], [])                                 ==> true
  ? ([], [1])                                ==> false
  ? ([1], [])                                ==> false
  ? ([1], [1])                               ==> true
  ? ([1], [2])                               ==> false
  ? ([1, 1], [1, 2])                         ==> false
  ? ([1, 2], [1, 1])                         ==> false
  ? ([1, 2], [1, 2])                         ==> true
  ? ([2, 1], [1, 2])                         ==> true
  ? ([1, 2, 3, 4, 5], [1, 2, 3, 4, 5])       ==> true
  ? ([1, 2, 3, 4, 5], [4, 5, 2, 1, 3])       ==> true
  ? ([1, 2, 3, 4, 5], [5, 4, 3, 2, 1])       ==> true
  ? ([1, 2, 3, 4, 5], [1, 2, 3, 4, 5, 6])    ==> false
  ? ([1, 2, 3, 4, 5, 6], [1, 2, 3, 4, 5])    ==> false
  ? ([1, 2, 3, 4, 5], [1, 2, 3, 6, 4, 5])    ==> false
  ? ([1, 2, 3, 6, 4, 5], [1, 2, 3, 4, 5])    ==> false
  ? ([1, 2, 3], [4, 5, 6])                   ==> false
  ? ([1, 1, 2, 2, 3, 3], [1, 2, 3])          ==> false
  ? ([1, 2, 3], [1, 1, 2, 2, 3, 3])          ==> false
  ? ([1, 2, 3, 1, 2, 3], [2, 3, 1, 1, 3, 2]) ==> true
  ? ([1, 2, 3, 1, 2, 3], [2, 3, 1, 2, 3, 2]) ==> false

  afproev antalPermutationer
  ? []                               ==> 1
  ? [1]                              ==> 1
  ? [1,2]                            ==> 2
  ? [1,1]                            ==> 1
  ? [1,2,3,4,5]                      ==> 120
  ? [1,2,2,3,5]                      ==> 60
  ? [1,1,1,2,2,3,4,4]                ==> 1680
  ? [1,2,3,4,1,2,4,1]                ==> 1680
  ? [1,1,1,1,1,1,1,1]                ==> 1
  ? [1,2,3,4,5,6,7,8,9,10,11,12]     ==> 479001600

  afproev antalPermutationerNy
  ? []                               ==> 1
  ? [1]                              ==> 1
  ? [1,2]                            ==> 2
  ? [1,1]                            ==> 1
  ? [1,2,3,4,5]                      ==> 120
  ? [1,2,2,3,5]                      ==> 60
  ? [1,1,1,2,2,3,4,4]                ==> 1680
  ? [1,2,3,4,1,2,4,1]                ==> 1680
  ? [1,1,1,1,1,1,1,1]                ==> 1
  ? [1,2,3,4,5,6,7,8,9,10,11,12]     ==> 479001600
  ? [1,2,3,4,5,6,7,8,9,10,12,12,12]  ==> 1037836800
  ? [1,1,1,2,2,3,4,4,4,4,5,5,5,5]    ==> 12612600
slut

opgave "5" "Højereordensfunktioner"
  note "Undersøg at der er brugt polymorfe højereordensfunktioner."
  note "Check at c) er løst vha. gentag."

  afproev grupper
  ? (fn n => n mod 10) & [] ==> []
  ? (fn n => n mod 10) & [1, 2, 3, 4] ==> [[1], [2], [3], [4]]
  ? (fn n => n mod 10) & [1, 2, 3, 4, 1, 2, 3, 4] :::
              dybpermutation [[1,1], [2,2], [3,3], [4,4]]
  ? (fn n => n mod 10) & [1, 11, 3, 5, 23, ~9] :::
              dybpermutation [[1,11,~9], [3, 23], [5]]

  afproev gentag
  ? tl & []                                   ==> []
  ? tl & [5]                                  ==> []
  ? tl & [1,2,3,4,5]                          ==> []
  ? (fn (x::xs) => List.drop(xs, x)) &
      [1,5, 2,4,5, 3,5,3,5, 2,1]              ==> [2, 1]
  ? (fn (x::xs) => List.drop(xs, x)) &
      [1,5, 2,4,5, 3,5,3,5, 2,1,6]            ==> []

  afproev gcd
  ? (0, 10)       ==> 10
  ? (10, 0)       ==> 10
  ? (2, 3)        ==> 1
  ? (4, 6)        ==> 2
  ? (650, 70)     ==> 10
  ? (7540, 41860) ==> 260
slut

opgave "6" "Generelle træer"
  note "Delopgave a) skal checkes manuelt."

  afproev praeorden
  ? K(1, []) ==> [1]
  ? K(1, [K(2, []), K(3, [])]) ==> [1, 2, 3]
  ? K(1, [K(2, [K(3, []), K(4, [])]),
          K(5, [K(6, []), K(7, [])])])
      ==> [1, 2, 3, 4, 5, 6, 7]
  ? K(3, [K(4, [K (7, []), K(1, []), K(5, [K(6, []), K(7, [])])])])
      ==> [3, 4, 7, 1, 5, 6, 7]
  ? K(1, [K(2, []), K(3, []), K(4, []), K(5, []), K(6, []), K(7, []), K(8, [])])
      ==> [1, 2, 3, 4, 5, 6, 7, 8]

  afproev erstat
  ? (K(1, []), [2]) ==> (K(2, []), [])
  ? (K(1, []), [2,3]) ==> (K(2, []), [3])
  ? (K(3, [K(4, [K(7, []), K(1, []), K(5, [K(6, []), K(7, [])])])]),
     [5,5,4,4,3,3,7,7,8,8,3,1,1,0]) ==>
    (K(5, [K(5, [K(4, []), K(4, []), K(3, [K(3, []), K(7, [])])])]),
     [7,8,8,3,1,1,0])
  ? (K(1, [K(2, [K(3, []), K(4, [])]), K(5, [])]), [9,8,7,6,5,4,3,2,1])
    ==> (K(9, [K(8, [K(7, []), K(6, [])]), K(5, [])]), [4,3,2,1])
  ? (K(3, [K(4, [K(7, []), K(1, []), K(5, [K(6, []), K(7, [])])])]),
     [1,2,3,4]) ::: kaster undtagelse

  afproev sorter
  ? K(1, []) ==> K(1, [])
  ? K(2, [K(1, []), K(4, []), K(3, [])]) ==>K(1, [K(2, []), K(3, []), K(4, [])])
  ? K(1, [K(2, []), K(3, []), K(4, []), K(5, []), K(6, []), K(7, []), K(8, [])])
      ==>
    K(1, [K(2, []), K(3, []), K(4, []), K(5, []), K(6, []), K(7, []), K(8, [])])
  ? K(5, [K(3, [K(7, []), K(9, []), K(8, [K(2, [K(3, [])])])]),
          K(2, [K(1, [K(3, []), K(5, [])])])]) ==>
    K(1, [K(2, [K(2, []), K(3, []), K(3, [K(3, [K(5, [])])])]),
          K(5, [K(7, [K(8, []), K(9, [])])])])
slut

opgave "7" "PPM"
  note "Husk at undersøge om filstrømme lukkes."

  med datafilerne "ppm"

  afproev inverterPPM
  ? fil "1px.ppm" & fil "1px-out.ppm"
        ::: giverFilen "1px-out.ppm"
             matchende "1px-inv.ppm"
  ? fil "comments.ppm" & fil "comments-out.ppm"
        ::: giverFilen "comments-out.ppm"
             matchende "comments-inv.ppm"
  ? fil "linebreaks.ppm" & fil "linebreaks-out.ppm"
        ::: giverFilen "linebreaks-out.ppm"
             matchende "linebreaks-inv.ppm"
  ? fil "simple.ppm" & fil "simple-out.ppm"
        ::: giverFilen "simple-out.ppm"
             matchende "simple-inv.ppm"
  ? fil "simple-nocomments.ppm" & fil "simple-nocomments-out.ppm"
        ::: giverFilen "simple-nocomments-out.ppm"
             matchende "simple-nocomments-inv.ppm"
  ? fil "rgb.ppm" & fil "rgb-out.ppm"
        ::: giverFilen "rgb-out.ppm"
             matchende "rgb-inv.ppm"
  ? fil "kort.ppm" & fil "kort-out.ppm"
        ::: giverFilen "kort-out.ppm"
             matchende "kort-inv.ppm"
slut

opgave "8" "Symboltabeller"
  note "Afprøvning af indsaet og find er kombineret"
  note "Check selv om tom er fornuftig."
  note "Husk at checke, at ListeTabel er implementeret med lister og FunTabel \
       \med funktioner"

  afproev listeIndsaet
  ? [("a", 6), ("b", 7), ("c", 8)]
      ::: checkListeTabel [("a", 6), ("b", 7), ("c", 8)]
  ? [("a", 6), ("b", 7), ("c", 8), ("b", 10)]
      ::: checkListeTabel [("a", 6), ("b", 10), ("c", 8)]
  ? [("a", 6), ("b", 7), ("c", 8), ("b", 10)]
      ::: checkIkkeListeTabel ["d"]
  ? [] ::: checkIkkeListeTabel ["a", "b", "c"]

  afproev funIndsaet
  ? [("a", 6), ("b", 7), ("c", 8)]
      ::: checkFunTabel [("a", 6), ("b", 7), ("c", 8)]
  ? [("a", 6), ("b", 7), ("c", 8), ("b", 10)]
      ::: checkFunTabel [("a", 6), ("b", 10), ("c", 8)]
  ? [("a", 6), ("b", 7), ("c", 8), ("b", 10)]
      ::: checkIkkeFunTabel ["d"]
  ? [] ::: checkIkkeFunTabel ["a", "b", "c"]
slut
heltslut
