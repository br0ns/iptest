signature Registrer =
sig
  val sekventialiseret2 : ('a -> 'b -> 'c) ->
                          'a * 'b -> 'c
  val sekventialiseret3 : ('a -> 'b -> 'c -> 'd) ->
                          ('a * 'b) * 'c -> 'd
  val sekventialiseret4 : ('a -> 'b -> 'c -> 'd -> 'e) ->
                          (('a * 'b) * 'c) * 'd -> 'e
  val sekventialiseret5 : ('a -> 'b -> 'c -> 'd -> 'e -> 'f) ->
                          ((('a * 'b) * 'c) * 'd) * 'e -> 'f
  val funktion : string ->
                 ('a -> 'b) ->
                 ('a -> Layout.t) ->
                 ('b -> Layout.t) ->
                 (string * ('a -> 'b) * ('a -> Layout.t) * ('b -> Layout.t)) * 'cs list
end
