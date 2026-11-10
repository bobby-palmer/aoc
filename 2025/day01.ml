open! Base

module Direction = struct

  type t =
    | Left of int
    | Right of int

  let of_string_exn s =
    Stdlib.Scanf.sscanf s "%c%d" (fun direction amount ->
      match direction with
      | 'L' -> Left amount
      | 'R' -> Right amount
      | _ -> raise_s [%message "bad direction" (direction : char)]
    )

end

let apply position = function
  | Direction.Left amount -> position - amount
  | Right amount -> position + amount

let pair elt = (elt, elt)

let simulate ~start_pos directions =
    start_pos :: (
      List.folding_map directions ~init:start_pos ~f:(fun position direction ->
        apply position direction |> pair
      )
    )

let parse input = 
  String.split_lines input 
  |> List.map ~f:Direction.of_string_exn
  |> simulate ~start_pos:50

let part1 locations = 
  List.filter locations ~f:(fun elt -> elt % 100 = 0) 
  |> List.length

let count_zeros start end_ =
  let section i =
    match i < 0 with
    | true -> (i - 99) / 100
    | false -> i / 100
  in
  let segments_crossed = 
    section (max start end_)
    - section (min start end_)
  in
  let adj =
    match start < end_ with
    | true -> 0
    | false -> 
        if start % 100 = 0 then -1 else 0
        + if end_ % 100 = 0 then 1 else 0
  in
  segments_crossed + adj

let rec part2 = function
  | a :: b :: xs ->
    count_zeros a b + part2 (b :: xs)
  | _ -> 0

let locations = 
  Aoc.Input.get ~year:2025 ~day:1 
  |> Or_error.ok_exn
  |> parse

let () = 
  Stdio.printf "part1: %d\n" (part1 locations);
  Stdio.printf "part2: %d\n" (part2 locations)
