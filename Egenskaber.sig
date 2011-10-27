signature Egenskaber =
sig
  type exn'
  type fil_egenskab

  val praedikat : string -> ('a -> bool) -> ('_, 'a) Test.egenskab
  val aekvivalens : string -> ('a * 'a -> bool) -> 'a -> ('_, 'a) Test.egenskab
  val eller : ('a, 'b) Test.egenskab * ('a, 'b) Test.egenskab ->
              ('a, 'b) Test.egenskab

  val ==> : 'a * ''b -> ('a, ''b) Test.tilfaelde
  val ~~> : 'a * real -> ('a, real) Test.tilfaelde
  val !!! : 'a * exn' -> ('a, 'b) Test.tilfaelde

  val lig : ''a -> ('_, ''a) Test.egenskab
  val reference : ('a -> ''b) -> ('a, ''b) Test.egenskab
  val circa : real -> ('_, real) Test.egenskab
  val parCirca : real * real -> ('_, real * real) Test.egenskab
  val permutation : ''a list -> ('_, ''a list) Test.egenskab
  val delliste : ''a list -> ('_, ''a list) Test.egenskab
  val kaster : exn' -> ('_, 'a) Test.egenskab
  val blandt : ''a list -> ('_, ''a) Test.egenskab
  val giverFilen : string -> fil_egenskab -> string -> ('_, 'a) Test.egenskab
  val indeholdende : fil_egenskab

  val Bind : exn'
  val Chr : exn'
  val Div : exn'
  val Domain : exn'
  val Empty : exn'
  val Fail : exn'
  val Interrupt : exn'
  val Match : exn'
  val Option : exn'
  val Overflow : exn'
  val Size : exn'
  val Subscript : exn'
  val Out_of_memory : exn'
  val undtagelse : exn'
end
