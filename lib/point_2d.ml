open! Core

module T = struct
  type t =
    { row : int
    ; col : int
    }
  [@@deriving sexp, compare, hash]
end

include T
include Comparable.Make (T)
include Hashable.Make (T)

let create ~row ~col = { row; col }
let row t = t.row
let col t = t.col
let north t = { t with row = t.row - 1 }
let west t = { t with col = t.col - 1 }
let south t = { t with row = t.row + 1 }
let east t = { t with col = t.col + 1 }
let neighbors t = [ north t; west t; south t; east t ]
let northwest = Fn.compose north west
let northeast = Fn.compose north east
let southwest = Fn.compose south west
let southeast = Fn.compose south east

let extended_neighbors t =
  [ northwest t; northeast t; southwest t; southeast t ] @ neighbors t
;;
