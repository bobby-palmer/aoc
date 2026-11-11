open! Base

type t =
  { row : int
  ; col : int
  }
[@@deriving compare]

let create ~row ~col = { row; col }
let north t = { t with row = t.row - 1 }
let west t = { t with col = t.col - 1 }
let south t = { t with row = t.row + 1 }
let east t = { t with col = t.col + 1 }
let cardinals = [ north; west; south; east ]
let northwest = Fn.compose north west
let northeast = Fn.compose north east
let southwest = Fn.compose south west
let southeast = Fn.compose south east
let extended_cardinals = [ northwest; northeast; southwest; southeast ] @ cardinals
