open! Base

type t [@@deriving compare]

val create : row:int -> col:int -> t
val north : t -> t
val west : t -> t
val south : t -> t
val east : t -> t
val cardinals : (t -> t) list
val northwest : t -> t
val northeast : t -> t
val southwest : t -> t
val southeast : t -> t
val extended_cardinals : (t -> t) list
