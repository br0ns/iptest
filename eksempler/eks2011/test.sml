saet "IP-eksamen 2011" er

opgave "1" "Stirlingtal"
  note "delopgave b) er en snakkeopgave."
  afproev stirling
  ? (0,0)    ==> 1
  ? (1,0)    ==> 0
  ? (2,0)    ==> 0
  ? (3,0)    ==> 0
  ? (4,0)    ==> 0
  ? (5,0)    ==> 0
  ? (6,0)    ==> 0
  ? (7,0)    ==> 0
  ? (8,0)    ==> 0
  ? (9,0)    ==> 0
  ? (10,0)   ==> 0
  ? (1,1)    ==> 1
  ? (2,1)    ==> 1
  ? (3,1)    ==> 1
  ? (4,1)    ==> 1
  ? (5,1)    ==> 1
  ? (6,1)    ==> 1
  ? (7,1)    ==> 1
  ? (8,1)    ==> 1
  ? (9,1)    ==> 1
  ? (10,1)   ==> 1
  ? (2,2)    ==> 1
  ? (3,2)    ==> 3
  ? (4,2)    ==> 7
  ? (5,2)    ==> 15
  ? (6,2)    ==> 31
  ? (7,2)    ==> 63
  ? (8,2)    ==> 127
  ? (9,2)    ==> 255
  ? (10,2)   ==> 511
  ? (3,3)    ==> 1
  ? (4,3)    ==> 6
  ? (5,3)    ==> 25
  ? (6,3)    ==> 90
  ? (7,3)    ==> 301
  ? (8,3)    ==> 966
  ? (9,3)    ==> 3025
  ? (10,3)   ==> 9330
  ? (4,4)    ==> 1
  ? (5,4)    ==> 10
  ? (6,4)    ==> 65
  ? (7,4)    ==> 350
  ? (8,4)    ==> 1701
  ? (9,4)    ==> 7770
  ? (10,4)   ==> 34105
  ? (5,5)    ==> 1
  ? (6,5)    ==> 15
  ? (7,5)    ==> 140
  ? (8,5)    ==> 1050
  ? (9,5)    ==> 6951
  ? (10,5)   ==> 42525
  ? (6,6)    ==> 1
  ? (7,6)    ==> 21
  ? (8,6)    ==> 266
  ? (9,6)    ==> 2646
  ? (10,6)   ==> 22827
  ? (7,7)    ==> 1
  ? (8,7)    ==> 28
  ? (9,7)    ==> 462
  ? (10,7)   ==> 5880
  ? (8,8)    ==> 1
  ? (9,8)    ==> 36
  ? (10,8)   ==> 750
  ? (9,9)    ==> 1
  ? (10,9)   ==> 45
  ? (10,10)  ==> 1
  ? (100, 50) ::: kaster Overflow
  ? (~1, 0)   ::: kaster Domain
  ? (0, ~1)   ::: kaster Domain
slut


opgave "2" "Trekant"
  afproev erNedreTrekant
  ? [[1], [1, 2], [1, 2, 3]] ==> true
  ? []                       ==> true
  ? [[1]]                    ==> true
  ? [[]]                     ==> false
  ? [[1], [2]]               ==> false
  ? [[1, 2], [3]]            ==> false
  ? [[], [1, 2]]             ==> false

  afproev diagonal
  ? []                       ==> []
  ? [[1]]                    ==> [1]
  ? [[1], [1, 2]]            ==> [1, 2]

  afproev vend
  ? []                       ==> []
  ? [[1]]                    ==> [[1]]
  ? [[1], [2, 3]]            ==> [[1, 2], [3]]
slut


opgave "3" "Højereordensfunktioner"
  note "Undersøg at der er brugt polymorfe højereordensfunktioner."
  note "Delopgave d) er en tidskompleksitetssnakkeopgave."
  afproev f
  ? [] & []                                 ==> []
  ? [1,2,3] & []                            ==> []
  ? [76, 23, 13, 54] & [4, 2]               ==> [54, 23]
  ? [76, 23, 13, 54] & [1, 1, 1]            ==> [76, 76, 76]
  ? [1, 2, 3, 4, 5] & [1, 2, 3, 4, 5]       ==> [1, 2, 3, 4, 5]

  afproev g
  ? [] & []                                 ==> []
  ? [] & [fn _ => true]                     ==> []
  ? [1, 2, 3, 4, 5] & [fn n => true]        ==> [1, 2, 3, 4, 5]
  ? [1, 2, 3, 4, 5] & [fn n => n mod 2 = 0] ==> [2, 4]
  ? [1, 2, 3, 4, 5] & [fn n => n mod 2 = 0,
                       fn n => n mod 3 = 0] ==> []
  ? [1, 2, 3, 4, 5] & [fn n => n mod 2 = 0,
                       fn n => n mod 4 = 0] ==> [4]

  afproev h
  ? [1.0]           ::: parCirca (1.0, 1.0)
  ? [1.0, 2.0]      ::: parCirca (1.0, 2.0)
  ? [2.0, 1.0]      ::: parCirca (1.0, 2.0)
  ? [2.0, 1.0, 3.0] ::: parCirca (1.0, 3.0)
  ? [1.0, 2.0, 1.0] ::: parCirca (1.0, 2.0)
  ? [1.0, 2.0, 0.0] ::: parCirca (0.0, 2.0)
  ? [2.0, 1.0, 2.0] ::: parCirca (1.0, 2.0)

slut

opgave "4" "Matricer"
  afproev isMatrix
  ? [[1]]                  ==> true
  ? [[1, 2], [3, 4]]       ==> true
  ? [[1, 2, 3], [4, 5, 6]] ==> true
  ? [[1], [2]]             ==> true
  ? [[1, 2]]               ==> true
  ? []                     ==> false
  ? [[]]                   ==> false
  ? [[], []]               ==> false
  ? [[], [1]]              ==> false
  ? [[1], [1, 2]]          ==> false
  ? [[1,2],[3],[4,5,6]]    ==> false

  afproev dim
  ? [[1]]                  ==> (1, 1)
  ? [[1, 2], [3, 4]]       ==> (2, 2)
  ? [[1, 2, 3], [4, 5, 6]] ==> (2, 3)

  afproev transpose
  ? [[1]]                  ==> [[1]]
  ? [[1, 2]]               ==> [[1], [2]]
  ? [[1], [2]]             ==> [[1, 2]]
  ? [[1, 2, 3], [4, 5, 6]] ==> [[1, 4], [2, 5], [3, 6]]

  afproev multiply
  ? ([[1]], [[1]])         ==> [[1]]
  ? ([[1]], [[1, 2]])      ==> [[1, 2]]
  ? ([[1], [1]], [[1, 1]]) ==> [[1, 1], [1, 1]]
  ? ([[1, 1]], [[1], [1]]) ==> [[2]]
  ? ([[4, 1, 3], [2, 0, 5]], [[4, 2], [1, 0], [3, 5]])
                           ==> [[26, 23], [23, 29]]
  ? ([[1, 1]], [[1, 1]])   ::: kaster Domain
  ? ([[1]], [[1], [1]])    ::: kaster Domain
  ? ([[1, 1]], [[1, 1]])   ::: kaster Domain
slut

opgave "5" "Burrows-Wheeler"
  note "Delopgave c) er afprøvning. Så husk at tjekke det."
  afproev BWT
  ? ""      ==> ""
  ? "a"     ==> "a"
  ? "ab"    ==> "ba"
  ? "ba"    ==> "ba"
  ? "azzcy" ==> "yzcza"

  afproev invBWT
  ? ("a", #"a")     ==> "a"
  ? ("ba", #"b")    ==> "ab"
  ? ("ba", #"a")    ==> "ba"
  ? ("yzcza", #"y") ==> "azzcy"
slut

opgave "6" "Træparsing"
  afproev gendan
  ? [<<,<<,<<,%,=="10",%,>>,=="-",<<,%,=="6",%,>>,>>,=="*",<<,%,=="2",%,>>,>>]
  ==> Gren (Gren (Gren (Blad, "10", Blad), "-", Gren (Blad, "6", Blad)),
            "*",
            Gren (Blad, "2", Blad))
  ? [%] ==> Blad
  ? [<<,%,=="foo",%,>>] ==> Gren (Blad, "foo", Blad)
slut

opgave "7" "Wordcount"
  note "Husk at undersøge om filstrømme lukkes."
  note "Husk at checke at der skrives til std_out i de rigtige tilfælde."

  hvor "fritz-random" indeholder
  " blib blob c1d -45$3 4Ad\n"

  og "tom-fil" indeholder
  ""

  og "linjeskift" indeholder
  "\n"

  og "snuskebasse" indeholder
  "hej snuskebasse\n"

  og "flere-linjer" indeholder
  "hej dig\n\
  \er du mig?\n\
  \nej\n\
  \okay\n"

  og "ingen-nl-til-sidst" indeholder
  "foo\n\
  \bar"

  afproev wordNum
  ? " blib blob c1d -45$3 4Ad\n"        ==> 5
  ? ""                                  ==> 0
  ? "\n"                                ==> 0
  ? "hej snuskebasse\n"                 ==> 2
  ? "hej dig\ner du mig?\nnej\nokay\n"  ==> 7
  ? "foo\nbar"                          ==> 2

  afproev wc1
  ? fil "fritz-random"       ==> (1, 5, 25)
  ? fil "tom-fil"            ==> (0, 0, 0)
  ? fil "linjeskift"         ==> (1, 0, 1)
  ? fil "snuskebasse"        ==> (1, 2, 16)
  ? fil "flere-linjer"       ==> (4, 7, 28)
  ? fil "ingen-nl-til-sidst" ==> (2, 2, 7)

  afproev display
  ? automagisk 1000 Vejl.display
  (intRange (0, 40) &` intRange (~10, 10))
  ? automagisk 1000 Vejl.display
  (intRange (0, 40) &` intRange (100000000, 200000000))
  ? automagisk 1000 Vejl.display
  (intRange (0, 40) &` intRange (~10000, 10000))

  afproev (wc $ fil "out")
  ? ([fil "fakeout"], SOME (fil "out"))
  ::: giverFilen "out" indeholdende
  (fil "fakeout" ^ " cannot be opened\n")

  ? ([fil "fritz-random"], SOME (fil "out"))
  ::: giverFilen "out" indeholdende
  ("       1       5      25 " ^ (fil "fritz-random") ^ "\n")

  ? (map fil ["ingen-nl-til-sidst", "tom-fil", "flere-linjer"]
   , SOME (fil "out"))
  ::: giverFilen "out" indeholdende
  ("       2       2       7 " ^ (fil "ingen-nl-til-sidst") ^ "\n" ^
   "       0       0       0 " ^ (fil "tom-fil") ^ "\n" ^
   "       4       7      28 " ^ (fil "flere-linjer") ^ "\n")

  ? ([], SOME (fil "out"))
  ::: giverFilen "out" indeholdende ""

slut

opgave "8" "Monoid"
  note "Husk at se om den 'rigtige' løsningsmetode er brugt."
  note "Delopgave a) kan ikke maskinafprøves."
  afproev pow
  ? ("", 0)        ==> ""
  ? ("a", 0)       ==> ""
  ? ("a", 5)       ==> "aaaaa"
  ? ("ab", 2)      ==> "abab"
slut

heltslut
