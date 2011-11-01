saet "Introduktion til Programmering 2010" er

opgave "1" "Fjern"
note "Delopgave b kan ikke maskinafprøves."
  afproev fjern
  ? #"t" & explode "klatret" ==> (explode "klaret", 3)
  ? #"a" & [#"a"]            ==> ([], 0)
  ? #"c" & explode "abc"     ==> (explode "ab", 2)
slut

opgave "2" "Udvælg"
note "Delopgave b kan ikke maskinafprøves."
  afproev udvaelg
  ? explode "tone" & [2, 3, 0, 0, 1] ==> explode "netto"
  ? explode "tone" & []              ==> []
  ? [] & []                          ==> []

  (* afproev udvaelg automagisk med Vejl.afproev *)
  (* >> list char & list int *)
  (* >> filtrer (fn (a & b) => *)
slut

opgave "3" "Sorter og permuter"
note "Delopgave b kan ikke maskinafprøves."
  afproev sortPerm
  ? [3.4, 1.7, 6.9, 2.1] ==> ([1, 3, 0, 2], [1.7, 2.1, 3.4, 6.9])
  ? []                   ==> ([], [])
  ? [1.0, 1.0, 1.0]      ::: blandt [([0, 1, 2], [1.0, 1.0, 1.0])
                                   , ([0, 2, 1], [1.0, 1.0, 1.0])
                                   , ([1, 0, 2], [1.0, 1.0, 1.0])
                                   , ([1, 2, 0], [1.0, 1.0, 1.0])
                                   , ([2, 0, 1], [1.0, 1.0, 1.0])
                                   , ([2, 1, 0], [1.0, 1.0, 1.0])
                                    ]
  ? automagisk med Vejl.sortPerm
  >> list real
slut

opgave "4" "Polynomier"
note "Husk at bemærke køretiden i delopgave c."
  afproev evalPoly
  ? [2, 0, 0, 1, ~18, ~3] & 2 ==> 29
  ? [] & 42                   ==> 0

  afproev visPoly
  ? [2, 0, 0, 1, ~18, ~3] ==> "2x^5+x^2-18x-3"
  ? []                    ==> "0"
  ? [1]                   ==> "1"
  ? [~1]                  ==> "-1"
  ? [~1, 3]               ==> "-x+3"
  ? [1, 0]                ==> "x"
slut

opgave "5" "Kalender"
hvor "nem.txt" indeholder
"11 08 2011 Jane & Svends sølvbryllup\n\
\23 10 2010 middag hos Aase\n\
\09 11 2010 Dansk Datahistorisk Forening\n\
\11 11 1968 Møde med JS\n"

og "ingen-nl-til-slut.txt" indeholder
"11 08 2011 Jane & Svends sølvbryllup\n\
\23 10 2010 middag hos Aase\n\
\09 11 2010 Dansk Datahistorisk Forening\n\
\11 11 1968 Møde med JS"

og "blank-linje.txt" indeholder
"11 08 2011 Jane & Svends sølvbryllup\n\
\23 10 2010 middag hos Aase\n\
\\n\
\09 11 2010 Dansk Datahistorisk Forening\n\
\11 11 1968 Møde med JS\n"

og "tocifret-aar.txt" indeholder
"11 08 2011 Jane & Svends sølvbryllup\n\
\23 10 2010 middag hos Aase\n\
\09 11 2010 Dansk Datahistorisk Forening\n\
\11 11 68 Møde med JS\n"

og "tocifret-aar-efter-2000.txt" indeholder
"11 08 2011 Jane & Svends sølvbryllup\n\
\23 10 2010 middag hos Aase\n\
\09 11 10 Dansk Datahistorisk Forening\n\
\11 11 1968 Møde med JS\n"

og "tabulator.txt" indeholder
"11\t08\t2011\tJane & Svends sølvbryllup\n\
\23\t10\t2010\tmiddag hos Aase\n\
\09\t11\t2010\tDansk Datahistorisk Forening\n\
\11\t11\t1968\tMøde med JS\n"

og "tabulator-og-mellemrum.txt" indeholder
"11\t08 2011\tJane & Svends sølvbryllup\n\
\23\t10 2010\tmiddag hos Aase\n\
\09\t11 2010\tDansk Datahistorisk Forening\n\
\11\t11 1968\tMøde med JS\n"

og "flere-blanktegn.txt" indeholder
"11  08\t 2011 Jane   &  Svends sølvbryllup\n\
\23 10\t\t2010 middag hos\t Aase\n\
\09 11 2010 \t Dansk Datahistorisk\tForening\n\
\11  11\t1968 Møde med JS\n"

og "tom.txt" indeholder "\n"

og "tom-uden-nl.txt" indeholder ""

  afproev hentKalender
  (* kaltjek er defineret i ekstra.sml og tilpasset de fire aftaler givet som
   * eksempel i eksamensopgaven
   *)
  ? fil "nem.txt"                     ::: kaltjek
  ? fil "ingen-nl-til-slut.txt"       ::: kaltjek
  ? fil "blank-linje.txt"             ::: kaltjek
  ? fil "tocifret-aar.txt"            ::: kaltjek
  ? fil "tocifret-aar-efter-2000.txt" ::: kaltjek
  ? fil "tabulator.txt"               ::: kaltjek
  ? fil "tabulator-og-mellemrum.txt"  ::: kaltjek
  ? fil "flere-blanktegn.txt"         ::: kaltjek
  ? fil "tom.txt"                     ==> []
  ? fil "tom-uden-nl.txt"             ==> []
slut

opgave "6" "Udsagnslogik"
note "NOT (NOT p) => p i simplify er bonus."
  afproev eval
  (* Valueringen tassign1 er givet i ekstra.sml *)
  ? VAR "The pope sleeps" & tassign1                ==> true
  ? VAR "The pope snores" & tassign1                ==> false
  ? VAR "The pope practices gymnastics" & tassign1  ==> false
  ? VAR "The pope preaches in his sleep" & tassign1 ==> true
  ? VAR "ERROR" & tassign1                          !!! Fail
  ? NOT (VAR "The pope sleeps") & tassign1          ==> false
  ? NOT (VAR "The pope snores") & tassign1          ==> true
  ? NOT TT & tassign1                               ==> false
  ? NOT FF & tassign1                               ==> true
  ? AND (TT, TT) & tassign1                         ==> true
  ? AND (FF, TT) & tassign1                         ==> false
  ? AND (TT, FF) & tassign1                         ==> false
  ? AND (FF, FF) & tassign1                         ==> false
  ? OR (TT, TT) & tassign1                          ==> true
  ? OR (FF, TT) & tassign1                          ==> true
  ? OR (TT, FF) & tassign1                          ==> true
  ? OR (FF, FF) & tassign1                          ==> false
  ? TT & tassign1                                   ==> true
  ? FF & tassign1                                   ==> false

  afproev implies
  ? (TT, TT) ::: impliestjek TT
  ? (TT, FF) ::: impliestjek FF
  ? (FF, TT) ::: impliestjek TT
  ? (FF, FF) ::: impliestjek TT

  afproev simplify
  ? TT                                   ::: simptjek TT
  ? FF                                   ::: simptjek FF
  ? VAR "a"                              ::: simptjek (VAR "a")
  ? AND (VAR "a", FF)                    ::: simptjek FF
  ? AND (FF, VAR "a")                    ::: simptjek FF
  ? AND (VAR "a", TT)                    ::: simptjek (VAR "a")
  ? AND (TT, VAR "a")                    ::: simptjek (VAR "a")
  ? AND (AND (VAR "a", NOT TT), VAR "b") ::: simptjek FF
  ? OR (VAR "a", TT)                     ::: simptjek TT
  ? OR (TT, VAR "a")                     ::: simptjek TT
  ? OR (VAR "a", FF)                     ::: simptjek (VAR "a")
  ? OR (FF, VAR "a")                     ::: simptjek (VAR "a")
  ? OR (AND (VAR "a", TT), NOT (OR (FF, NOT (VAR "b"))))
                                         ::: simptjek (OR (VAR "a", VAR "b"))
slut

opgave "7" "Klassedeling"
note "Delopgave c kan ikke maskinafprøves."
  afproev klassedelinger
  (* Funktionen klassetjek er givet i ekstra.sml *)
  ? []           ==> [[]]
  ? [1]          ==> [[[1]]]
  ? [1, 2]       :::
  klassetjek [[[1, 2]], [[1], [2]]]
  ? [1, 2, 3]    :::
  klassetjek [[[1, 2, 3]],

              [[1], [2, 3]],
              [[2], [1, 3]],
              [[3], [1, 2]],

              [[1], [2], [3]]]
  ? [1, 2, 3, 4] :::
  klassetjek [[[1, 2, 3, 4]],

              [[1], [2, 3, 4]],
              [[2], [1, 3, 4]],
              [[3], [1, 2, 4]],
              [[4], [1, 2, 3]],

              [[1, 2], [3, 4]],
              [[1, 3], [2, 4]],
              [[1, 4], [2, 3]],

              [[1], [2], [3, 4]],
              [[1], [3], [2, 4]],
              [[1], [4], [2, 3]],
              [[2], [3], [1, 4]],
              [[2], [4], [1, 3]],
              [[3], [4], [1, 2]],

              [[1], [2], [3], [4]]]

  ? automagisk med Vejl.klassedelinger
  >> list int

  afproev bell
  ? 0  ==> 1
  ? 1  ==> 1
  ? 2  ==> 2
  ? 3  ==> 5
  ? 4  ==> 15
  ? 5  ==> 52
  ? 6  ==> 203
  ? 7  ==> 877
  ? 8  ==> 4140
  ? 9  ==> 21147
  ? 10 ==> 115975
  ? 11 ==> 678570
  ? 12 ==> 4213597
  ? 13 ==> 27644437
  ? 14 ==> 190899322
slut

opgave "8" "K-lister"
note "Delopgaver a og c kan ikke afprøves maskinelt."
  afproev fraListe
  (* Funktionen klisttjek er givet i ekstra.sml *)
  ? []        ::: klisttjek
  ? [1, 2, 3] ::: klisttjek
  ? [1, 1]    ::: klisttjek
slut

opgave "9" "Permutationsnummering"
note "Den sidste afprøvning kan tage meget lang tid, hvis opgaven er løst naïvt."
  afproev permNr
  ? 0 & 0  ==> []

  ? 1 & 0  ==> [0]

  ? 2 & 0  ==> [1, 0]
  ? 2 & 1  ==> [0, 1]

  ? 3 & 0  ==> [2, 1, 0]
  ? 3 & 1  ==> [1, 2, 0]
  ? 3 & 2  ==> [1, 0, 2]
  ? 3 & 3  ==> [2, 0, 1]
  ? 3 & 4  ==> [0, 2, 1]
  ? 3 & 5  ==> [0, 1, 2]

  ? 4 & 0  ==> [3, 2, 1, 0]
  ? 4 & 1  ==> [2, 3, 1, 0]
  ? 4 & 2  ==> [2, 1, 3, 0]
  ? 4 & 3  ==> [2, 1, 0, 3]

  ? 4 & 4  ==> [3, 1, 2, 0]
  ? 4 & 5  ==> [1, 3, 2, 0]
  ? 4 & 6  ==> [1, 2, 3, 0]
  ? 4 & 7  ==> [1, 2, 0, 3]

  ? 4 & 8  ==> [3, 1, 0, 2]
  ? 4 & 9  ==> [1, 3, 0, 2]
  ? 4 & 10 ==> [1, 0, 3, 2]
  ? 4 & 11 ==> [1, 0, 2, 3]

  ? 4 & 12 ==> [3, 2, 0, 1]
  ? 4 & 13 ==> [2, 3, 0, 1]
  ? 4 & 14 ==> [2, 0, 3, 1]
  ? 4 & 15 ==> [2, 0, 1, 3]

  ? 4 & 16 ==> [3, 0, 2, 1]
  ? 4 & 17 ==> [0, 3, 2, 1]
  ? 4 & 18 ==> [0, 2, 3, 1]
  ? 4 & 19 ==> [0, 2, 1, 3]

  ? 4 & 20 ==> [3, 0, 1, 2]
  ? 4 & 21 ==> [0, 3, 1, 2]
  ? 4 & 22 ==> [0, 1, 3, 2]
  ? 4 & 23 ==> [0, 1, 2, 3]

  ? 10 & 50000 ==> [9, 3, 4, 2, 7, 5, 8, 1, 0, 6]
slut

heltslut
