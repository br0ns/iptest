signature TestDSL =
sig
  (* infixr 0 slut er
   * infix 1 afproev note hvor og
   * infix 2 ? indeholder
   * infix 3 ::: & ==> ~~> !!!
   * infix 4 eller
   *)

  val saet : 'a -> 'a
  val er : string * Test.opgave list -> unit

  val slut : 'a * 'a list -> 'a list
  val heltslut : 'a list

  val afproev : Test.opgave * ('a, 'b) Test.afproevning ->
                Test.opgave

  val note : Test.opgave * string -> Test.opgave
  val hvor : Test.opgave * 'a -> Test.opgave
  val og : Test.opgave * 'a -> Test.opgave
  val indeholder : string * string -> unit
  val fil : string -> string

  val opgave : string -> string -> Test.opgave

  val ? : ('a, 'b) Test.afproevning * ('a * ('a, 'b) Test.egenskab) ->
          ('a, 'b) Test.afproevning
  val & : 'a * 'b -> 'a * 'b

  val ::: : 'a * 'b -> 'a * 'b

  include Egenskaber
end
