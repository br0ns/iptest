signature Vis =
sig
  type 'a t = 'a -> Layout.t
  val unit : unit t
  val int : int t
  val real : real t
  val bool : bool t
  val string : string t
  val char : char t
  val order : order t
  val list : 'a t -> 'a list t
  val option : 'a t -> 'a option t

  val par : 'a t * 'b t -> ('a * 'b) t

  val triple : 'a t * 'b t * 'c t ->
               ('a * 'b * 'c) t

  val tupel4 : 'a t * 'b t * 'c t * 'd t ->
               ('a * 'b * 'c * 'd) t

  val tupel5 : 'a t * 'b t * 'c t * 'd t * 'e t ->
               ('a * 'b * 'c * 'd * 'e) t
end
