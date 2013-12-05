saet "IP-eksamen 2013" er

opgave "1" "Option lister"
  afproev firstDefined
  ? [SOME 49, SOME 21, SOME 45]                    ==> SOME 49
  ? [SOME 61, SOME 31, NONE, SOME 21]              ==> SOME 61
  ? [SOME 42]                                      ==> SOME 42
  ? [NONE, SOME 7, SOME 3, NONE]                   ==> SOME 7
  ? []                                             ==> NONE
  ? [NONE]                                         ==> NONE
  ? [NONE, NONE]                                   ==> NONE
  ? [NONE, NONE, NONE]                             ==> NONE
  ? [NONE, NONE, NONE, NONE, NONE, NONE, NONE]     ==> NONE
  ? [NONE, NONE, NONE, NONE, NONE, SOME 451, NONE] ==> SOME 451

  afproev allDefined
  ? []                                   ==> []
  ? [SOME 1]                             ==> [1]
  ? [SOME 5, SOME 6, SOME 7, SOME 9]     ==> [5, 6, 7, 9]
  ? [SOME 3, NONE, SOME 12]              ==> [3, 12]
  ? [NONE]                               ==> []
  ? [NONE, NONE]                         ==> []
  ? [NONE, NONE, NONE, NONE, NONE]       ==> []
  ? [NONE, NONE, NONE, NONE, SOME 4]     ==> [4]
  ? [SOME 9, NONE, NONE, NONE, NONE]     ==> [9]
  ? [SOME 9, NONE, NONE, SOME 3, NONE]   ==> [9, 3]
  ? [SOME 3, SOME 45, SOME 12, NONE]     ==> [3, 45, 12]
  ? [NONE, SOME 7, SOME 3, NONE, SOME 7] ==> [7, 3, 7]
slut

opgave "2" "Deltegnfølger"
  afproev subsequence
  ? ("abba", "abracadabra")  ==> true
  ? ("aba", "aba")           ==> true
  ? ("aab", "abba")          ==> false
  ? ("", "tetris")           ==> true
  ? ("tetris", "")           ==> false
  ? ("", "")                 ==> true
  ? ("tennis", "tennis")     ==> true
  ? ("abcdefgh", "achgbdef") ==> false
  ? ("acfh", "abcdefgh")     ==> true
  ? ("bcdef", "abcdefgh")    ==> true
  ? ("a", "x")               ==> false
slut

opgave "3" "Cifferdivisable tal"
  afproev digitDivisible
  ? ~81      ==> false
  ? ~15      ==> false
  ? ~14      ==> false
  ? ~1       ==> false
  ?  0       ==> false
  ?  1       ==> true
  ?  2       ==> true
  ?  3       ==> true
  ?  4       ==> true
  ?  5       ==> true
  ?  6       ==> true
  ?  7       ==> true
  ?  8       ==> true
  ?  9       ==> true
  ? 10       ==> false
  ? 11       ==> true
  ? 12       ==> true
  ? 13       ==> false
  ? 14       ==> false
  ? 15       ==> true
  ? 16       ==> false
  ? 17       ==> false
  ? 18       ==> false
  ? 19       ==> false
  ? 20       ==> false
  ? 21       ==> false
  ? 22       ==> true
  ? 23       ==> false
  ? 24       ==> true
  ? 25       ==> false
  ? 26       ==> false
  ? 27       ==> false
  ? 28       ==> false
  ? 29       ==> false
  ? 30       ==> false
  ? 31       ==> false
  ? 32       ==> false
  ? 33       ==> true
  ? 34       ==> false
  ? 35       ==> false
  ? 36       ==> true
  ? 37       ==> false
  ? 38       ==> false
  ? 39       ==> false
  ? 40       ==> false
  ? 41       ==> false
  ? 42       ==> false
  ? 43       ==> false
  ? 44       ==> true
  ? 45       ==> false
  ? 46       ==> false
  ? 47       ==> false
  ? 48       ==> true
  ? 49       ==> false
  ? 97       ==> false
  ? 99       ==> true
  ? 101      ==> false
  ? 315      ==> true
  ? 567      ==> false
  ? 1110     ==> false
  ? 3636     ==> true
  ? 428424   ==> true
  ? 428444   ==> false
  ? ~9       ==> false

  afproev digitDivisibleBetween
  ? (10, 30)         ==> [11, 12, 15, 22, 24]
  ? (25, 30)         ==> []
  ? (30, 10)         ==> []
  ? (~100, 0)        ==> []
  ? (0, ~100)        ==> []
  ? (0, 50)          ==> [1,2,3,4,5,6,7,8,9,11,12,15,22,24,33,36,44,48]
  ? (50, 0)          ==> []
  ? (4521, 4570)     ==> []
  ? (428422, 428432) ==> [428424]
  ? (1009, 1011)     ==> []
  ? (11, 11)         ==> [11]
  ? (11, 12)         ==> [11, 12]
  ? (90, 110)        ==> [99]
slut

opgave "4" "Højereordensfunktioner"
  note "Husk at checke, at opgaverne er løst med listekombinatorer."

  afproev countdown
  ? 0    ==> [0]
  ? 1    ==> [1, 0]
  ? 2    ==> [2, 1, 0]
  ? 3    ==> [3, 2, 1, 0]
  ? 4    ==> [4, 3, 2, 1, 0]
  ? 5    ==> [5, 4, 3, 2, 1, 0]
  ? 6    ==> [6, 5, 4, 3, 2, 1, 0]
  ? 7    ==> [7, 6, 5, 4, 3, 2, 1, 0]
  ? 8    ==> [8, 7, 6, 5, 4, 3, 2, 1, 0]
  ? 9    ==> [9, 8, 7, 6, 5, 4, 3, 2, 1, 0]
  ? ~1   ::: kaster Domain
  ? ~10  ::: kaster Domain
  ? ~31  ::: kaster Domain

  afproev orderedList
  ? []            ==> true
  ? [100]         ==> true
  ? [2,2]         ==> true
  ? [2,3]         ==> false
  ? [1,2,3,4,5]   ==> false
  ? [5,4,3,2,1]   ==> true
  ? [3,2,2,2,1]   ==> true
  ? [8,5,5,3]     ==> true
  ? [3,5,8,5]     ==> false
  ? [1,2,2,4,2,3] ==> false
  ? [6,3,1,4,5]   ==> false
  ? [valOf Int.minInt, 2, 1] ==> false
  ? [~5, ~10]     ==> true
  ? [10, 0, ~10, ~42] ==> true
  ? [~10, ~5]     ==> false

  afproev nns
  ? 0  ==> []
  ? 1  ==> [1]
  ? 2  ==> [1, 2, 2]
  ? 3  ==> [1, 2, 2, 3, 3, 3]
  ? 4  ==> [1, 2, 2, 3, 3, 3, 4, 4, 4, 4]
  ? 5  ==> [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5]
  ? 6  ==> [1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6]
  ? ~1 ::: kaster Domain
  ? ~10 ::: kaster Domain
slut

opgave "5" "SVG"
  note "Svær at afprøve automatisk. Hvis tests'ne fejler, afprøv manuelt."

  med datafilerne "svg"

  afproev drawingToSVG
  ? fil "empty-out.svg" & []
      ::: giverFilen "empty-out.svg"
           matchende "empty.svg"
  ? fil "single-point-out.svg" & [(100, 100)]
      ::: giverFilen "single-point-out.svg"
           matchende "single-point.svg"
  ? fil "problemset-out.svg" & [(0,0), (100,100), (200, 0)]
      ::: giverFilen "problemset-out.svg"
           matchende "problemset.svg"
  ? fil "somelines-out.svg" & [(0,0), (10, 10), (0,20), (10, 30), (0, 40)]
      ::: giverFilen "somelines-out.svg"
           matchende "somelines.svg"
  ? fil "neg-out.svg" & [(0,0), (~10, 10), (~10, ~20)]
      ::: giverFilen "neg-out.svg"
           matchende "neg.svg"
slut

opgave "6" "LOGO"
  note "(a) skal checkes manuelt"
  note "Vær obs på, at nogle opgaver kan fejle pga ekstra optimeringer"

  afproev optimise
  ? []                                ==> []
  ? [Turn]                            ==> [Turn]
  ? [Move 10]                         ==> [Move 10]
  ? [Repeat (4, [Move 5, Turn])]      ==> [Repeat (4, [Move 5, Turn])]
  ? [Turn, Turn, Turn, Turn]          ==> []
  ? [Turn, Turn, Turn, Turn, Turn]    ==> [Turn]
  ? [Move 10, Turn, Turn, Turn, Turn] ==> [Move 10]
  ? [Turn, Turn, Turn, Turn, Move 10] ==> [Move 10]
  ? [Move 10, Move 56]                ==> [Move 66]
  ? [Move 10, Move ~10]               ==> [Move 10, Move ~10]
  ? [Turn, Move 1, Turn, Turn, Move 2, Move 3, Turn]
                                  ==> [Turn, Move 1, Turn, Turn, Move 5, Turn]
  ? [Move 0]                          ==> []
  ? [Repeat (0, [Move 10])]           ==> []
  ? [Move 1, Move 2, Move 3]          ==> [Move 6]
  ? [Move 10, Turn, Turn, Turn, Turn, Move 10] ==> [Move 20]
  ? [Turn, Turn, Move 1, Repeat (0, [Turn]), Move ~1, Turn, Turn, Turn, Move 30]
      ==> [Turn, Turn, Move 1, Move ~1, Turn, Turn, Turn, Move 30]
  ? [Move 20, Turn, Turn, Turn, Repeat (0, [Turn]), Turn, Move 30]
                                      ==> [Move 50]
  ? [Repeat (2, [Move 3, Move 1, Turn, Move 0]), Move 5, Move 6]
                                      ==> [Repeat (2, [Move 4, Turn]), Move 11]

  afproev removeRepeats
  ? []                                ==> []
  ? [Turn]                            ==> [Turn]
  ? [Turn, Turn]                      ==> [Turn, Turn]
  ? [Turn, Turn, Turn]                ==> [Turn, Turn, Turn]
  ? [Repeat (0, [])]                  ==> []
  ? [Repeat (200000, []), Turn]       ==> [Turn]
  ? [Repeat (5, [Turn])]              ==> [Turn, Turn, Turn, Turn, Turn]
  ? [Repeat (4, [Move 10, Turn])]     ==> [Move 10, Turn, Move 10, Turn,
                                           Move 10, Turn, Move 10, Turn]
  ? [Repeat (2, [Repeat (2, [Move 5, Turn])])] ==>
      [Move 5, Turn, Move 5, Turn, Move 5, Turn, Move 5, Turn]
  ? [Move 10, Repeat(2, [Turn]), Move 50] ==> [Move 10, Turn, Turn, Move 50]
  ? [Move 10, Turn, Turn, Move 5, Repeat(3, [Move 10, Turn, Move 3, Turn]),
     Move 5, Turn, Turn, Move 10]  ==>
      [Move 10, Turn, Turn, Move 5, Move 10, Turn, Move 3, Turn, Move 10, Turn,
       Move 3, Turn, Move 10, Turn, Move 3, Turn, Move 5, Turn, Turn, Move 10]

  afproev eval
  ? (((0, 0), North), [])                              ==> [(0, 0)]
  ? (((0, 0), South), [])                              ==> [(0, 0)]
  ? (((0, 0), West), [])                               ==> [(0, 0)]
  ? (((0, 0), East), [])                               ==> [(0, 0)]
  ? (((10, ~10), East), [])                            ==> [(10, ~10)]
  ? (((10, 10), North), [Turn])                        ==> [(10, 10)]
  ? (((10, 10), North), [Move 10])                     ==> [(10, 10), (10, 20)]
  ? (((10, 10), South), [Move 10])                     ==> [(10, 10), (10, 0)]
  ? (((10, 10), East),  [Move 10])                     ==> [(10, 10), (20, 10)]
  ? (((10, 10), West),  [Move 10])                     ==> [(10, 10), (0, 10)]
  ? (((10, 10), North), [Turn, Move 10])               ==> [(10, 10), (20, 10)]
  ? (((10, 10), South), [Turn, Move 10])               ==> [(10, 10), (0, 10)]
  ? (((10, 10), East),  [Turn, Move 10])               ==> [(10, 10), (10, 0)]
  ? (((10, 10), West),  [Turn, Move 10])               ==> [(10, 10), (10, 20)]
  ? (((10, 10), North), [Repeat(0, [Move 10])])        ==> [(10, 10)]
  ? (((20, 50), North), [Repeat (5, [Move 10, Turn])])
      ==> [(20, 50), (20, 60), (30, 60), (30, 50), (20, 50), (20, 60)]
  ? (((1, 1), North), [Turn, Move 3, Turn, Move 2, Turn, Move ~1, Move 2])
      ==> [(1, 1), (4, 1), (4, ~1), (5, ~1), (3, ~1)]

  afproev singleTurnsOnly
  ? []                                       ==> []
  ? [Move 20]                                ==> [Move 20]
  ? [Move ~20]                               ==> [Move ~20]
  ? [Turn]                                   ==> [Turn]
  ? [Turn, Move 10]                          ==> [Turn, Move 10]
  ? [Turn, Turn]                             ==> []
  ? [Move 10, Turn, Turn]                    ==> [Move 10]
  ? [Move ~10, Turn, Turn]                   ==> [Move ~10]
  ? [Turn, Turn, Move 10]                    ==> [Move ~10]
  ? [Turn, Turn, Move ~10]                   ==> [Move 10]
  ? [Move 10, Turn, Turn, Move 10]           ==> [Move 10, Move ~10]
  ? [Move 10, Turn, Turn, Move 10, Move ~10] ==> [Move 10, Move ~10, Move 10]
  ? [Repeat (2, [Turn, Turn, Move 10])]      ==> [Move ~10, Move 10]
  ? [Repeat(4, [Move 1])]  ::: blandt [ [Repeat(4, [Move 1])],
                                        [Move 1, Move 1, Move 1, Move 1],
                                        [Move 4] ]
slut

opgave "7" "Sub-list sum"
  afproev subListSum
  ? (0, [])                  ==> SOME []
  ? (1, [])                  ==> NONE
  ? (4, [1, 5, 5, 1, 5, 1, 1]) ==> SOME [1, 1, 1, 1]
  ? (10, [10])               ==> SOME [10]
  ? (20, [10, 20, 30])       ==> SOME [20]
  ? (20, [10, 10, 30])       ==> SOME [10, 10]
  ? (7, [4, ~2, 3, 5])       ::: blandt [SOME [4, 3], SOME [4, ~2, 5]]
  ? (~1, [4, ~2, 3, 5])      ==> NONE
  ? (10, [5, 1, 5, 7, 9, 2]) ::: blandt [SOME [1, 9], SOME [5, 5], SOME [1,7,2]]
  ? (19, [5, 1, 5, 7, 9, 2]) ::: blandt [ SOME [5, 5, 9],
                                          SOME [1, 7, 9, 2],
                                          SOME [5, 5, 7, 2] ]
  ? (10, [2, ~1, 6, 7, 2, ~4]) ::: blandt [ SOME [2, 6, 2],
                                            SOME [2, ~1, 7, 2],
                                            SOME [~1, 6, 7, 2, ~4],
                                            SOME [2, ~1, 6, 7, ~4] ]
  ? (100, [100,324,324,543,645,214,876,234,234,876,214,345,343,543,645,234,345,
           6435,654,234,6465,234,123,543,345,4234,21]) ==> SOME [100]
slut

heltslut
