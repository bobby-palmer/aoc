open Aoc

let parse s =
  Scanf.sscanf s "%d,%d,%d" (fun a b c -> (a, b, c))

let boxes =
  Input.get ~year:2025 ~day:8
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse

let squared num = num * num

let dist2 (a, b, c) (x, y, z) =
  squared (a - x) + squared (b - y) + squared (c - z)

let rec pairs = function
  | x :: xs ->
      let using_this = xs
        |> List.map (fun elt -> (x, elt)) in
      using_this @ (pairs xs)
  | [] -> []
