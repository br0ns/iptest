signature ProgressBar =
sig
  type t
  val progress : t -> real -> t
  val alert : t -> t
  val init : int -> t
end
