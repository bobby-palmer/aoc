open! Core

type t [@@deriving sexp, compare, hash]

include Comparable.S with type t := t
include Hashable.S with type t := t

val create : row:int -> col:int -> t
val row : t -> int
val col : t -> int
val north : t -> t
val west : t -> t
val south : t -> t
val east : t -> t
val neighbors : t -> t list
val northwest : t -> t
val northeast : t -> t
val southwest : t -> t
val southeast : t -> t
val extended_neighbors : t -> t list
