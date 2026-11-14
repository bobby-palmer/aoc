open! Core

type t

val of_string : string -> t
val where_letter_is : t -> letter:char -> Point_2d.t list
val to_list : t -> (Point_2d.t * char) list
val num_rows : t -> int
val num_cols : t -> int
