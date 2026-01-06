type t = { x: int; y: int; }

let create x y = { x; y }
let of_tuple (x, y) = { x; y }
let to_tuple v = (v.x, v.y)

let add a b = { x = a.x + b.x; y = a.y + b.y }
let sub a b = { x = a.x - b.x; y = a.y - b.y }
let scale factor v = { x = v.x * factor; y = v.y * factor }

let zero = { x = 0; y = 0 }

let north = { x = 0; y = 1 }
let south = { x = 0; y = -1 }
let west = { x = -1; y = 0 }
let east = { x = 1; y = 0 }

let cardinals = [
  north;
  south;
  west;
  east
]

let northwest = add north west
let northeast = add north east
let southwest = add south west
let southeast = add south east

let diagonals = [
  northwest;
  northeast;
  southwest;
  southeast
]

let directions = cardinals @ diagonals

let manhattan_distance a b =
  abs (a.x - b.x) + abs (a.y - b.y)
