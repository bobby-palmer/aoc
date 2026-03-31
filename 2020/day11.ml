open Aoc

type square = Floor | Empty | Taken

let square_of_char = function
  | '.' -> Floor
  | 'L' -> Empty
  | '#' -> Taken
  | _ -> failwith "bad char"

let input =
  Input.get ~year:2020 ~day:11
  |> Grid.of_string
  |> Grid.map square_of_char
