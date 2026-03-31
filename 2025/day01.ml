open Aoc

let parse line =
  Scanf.sscanf line "%c%d" (fun direction amount -> 
    match direction with
    | 'L' -> (-1) * amount
    | 'R' -> amount
    | _ -> failwith "bad input"
  )

(** Positive mod *)
let ( %! ) a b =
  let abs_ans = a mod b in
  if abs_ans < 0 then 
    abs_ans + b
  else 
    abs_ans

let directions =
  Input.get ~year:2025 ~day:1 |>
  String.split_on_char '\n' |>
  List.map parse

let rec count_zero pos = function
  | x :: xs ->
      let is_zero =
        if pos = 0 then 1
        else 0
      in
      let next_pos = (pos + x) %! 100 in
      is_zero + count_zero next_pos xs
  | [] -> 0

let () =
  let zeros = count_zero 50 directions in
  Printf.printf "Part1: %d\n" zeros

let count_intermediate_zeros pos move =
  let normalized_right =
    if move > 0 then
      pos + move
    else
      ((pos + move) %! 100) - move
  in
  if normalized_right mod 100 = 0 then
    normalized_right / 100 - 1
  else 
    normalized_right / 100

let rec count_zero pos = function
  | x :: xs ->
      let next_pos = (pos + x) %! 100 in
      let zeros_for_move = 
        if next_pos = 0 then
          1 + count_intermediate_zeros pos x
        else
          count_intermediate_zeros pos x 
      in
      zeros_for_move + count_zero next_pos xs
  | [] -> 0

let () =
  let zeroes = count_zero 50 directions in
  Printf.printf "Part2: %d\n" zeroes
