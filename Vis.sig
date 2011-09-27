signature Vis =
sig
  val unit : unit -> Layout.t
  val int : int -> Layout.t
  val real : real -> Layout.t
  val bool : bool -> Layout.t
  val string : string -> Layout.t
  val char : char -> Layout.t
  val order : order -> Layout.t
  val list : ('a -> Layout.t) -> 'a list -> Layout.t
  val option : ('a -> Layout.t) -> 'a option -> Layout.t

  val par : ('a -> Layout.t) * ('b -> Layout.t) -> 'a * 'b -> Layout.t

  val triple : ('a -> Layout.t) * ('b -> Layout.t) * ('c -> Layout.t) ->
               'a * 'b * 'c -> Layout.t

  val tupel4 : ('a -> Layout.t) * ('b -> Layout.t) * ('c -> Layout.t) *
               ('d -> Layout.t) ->
               'a * 'b * 'c * 'd -> Layout.t

  val tupel5 : ('a -> Layout.t) * ('b -> Layout.t) * ('c -> Layout.t) *
               ('d -> Layout.t) * ('e -> Layout.t) ->
               'a * 'b * 'c * 'd * 'e -> Layout.t
end
