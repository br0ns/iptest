signature Test =
sig
  exception Tom
  type resultat
  datatype 'a forventning = Vaerdi of 'a * string
                          | Beskrivelse of string

  type ('a, 'b) funktion = string * ('a -> 'b) * 'a Vis.t * 'b Vis.t
  type ('a, 'b) egenskab = 'a -> ('b Lazy.t -> bool) * 'b forventning list
  type ('a, 'b) tilfaelde = ('a * ('a, 'b) egenskab) list
  type ('a, 'b) afproevning = ('a, 'b) funktion * ('a, 'b) tilfaelde
  type opgave = (string * string * string option) * resultat Lazy.t list

  val tjek : ('a, 'b) afproevning -> resultat
  val udskriv : string * opgave list -> unit
end
