type t = (int * int)

let compare = compare

type direction =
  | North
  | NorthWest
  | NorthEast
  | South
  | SouthWest
  | SouthEast
  | West
  | East

let directions = [
  North;
  NorthWest;
  NorthEast;
  South;
  SouthWest;
  SouthEast;
  East;
  West;
]

let apply (r, c) = function
  | North -> (r - 1, c)
  | NorthWest -> (r - 1, c - 1)
  | NorthEast -> (r - 1, c + 1)
  | South -> (r + 1, c)
  | SouthWest -> (r + 1, c - 1)
  | SouthEast -> (r + 1, c + 1)
  | West -> (r, c - 1)
  | East -> (r, c + 1)

let neighbors point = List.map (apply point) directions
