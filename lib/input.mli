open! Base

val get : year:int -> day:int -> string Or_error.t
val get_exn : year:int -> day:int -> string
