open Aoc

let int_of_char ch =
  int_of_char ch - int_of_char '0'

let parse_bank s =
  s |>
  String.to_seq |>
  Seq.map int_of_char |>
  Array.of_seq

let banks =
  Input.get ~year:2025 ~day:3 |>
  String.split_on_char '\n' |>
  List.map parse_bank

let get_max_volts to_use batteries =
  let rec aux to_use acc batteries =
    if to_use = 0 then
      acc
    else
      let num_options = 
        Array.length batteries - to_use + 1 in
      let options = Array.sub batteries 0 num_options in
      let best_option = Array.fold_left max 0 options in
      let best_i = Array.find_index (( = ) best_option) options 
        |> Option.get in
      let count_remaining = Array.length batteries - best_i - 1 in
      let remaining = 
        Array.sub batteries (best_i + 1) count_remaining in
      aux (to_use - 1) (10*acc + best_option) remaining
  in
  aux to_use 0 batteries

let sum_maxes count_to_use banks =
  banks |>
  List.map (get_max_volts count_to_use) |>
  List.fold_left ( + ) 0

let () =
  let big = sum_maxes 2 banks in
  Printf.printf "Part1: %d\n" big

let () =
  let big = sum_maxes 12 banks in
  Printf.printf "Part2: %d\n" big

