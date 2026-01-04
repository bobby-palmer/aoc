type t = int * int

let compare = compare

let add (x, y) (u, v) = (x + u, y + v)

let scale factor (x, y) = (x * factor, y * factor)

type cardinal = North | South | West | East

let unit_of_cardinal = function
  | North -> (0, 1)
  | South -> (0, -1)
  | West -> (-1, 0)
  | East -> (1, 0)
