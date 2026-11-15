open! Core
open! Aoc

let tiles =
  let parse line = Scanf.sscanf line "%d,%d" (fun a b -> a, b) in
  Input.get ~year:2025 ~day:9
  |> Or_error.ok_exn
  |> String.split_lines
  |> List.map ~f:parse
;;

let part1 =
  let area (x, y) (u, v) = (abs (x - u) + 1) * (abs (y - v) + 1) in
  let rec aux = function
    | x :: xs ->
      let this_area = List.map xs ~f:(area x) |> List.fold ~init:0 ~f:max in
      max this_area (aux xs)
    | _ -> 0
  in
  aux tiles
;;

let () = printf "Part1: %d\n" part1
