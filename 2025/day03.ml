open Aoc

let int_of_char ch =
  int_of_char ch - int_of_char '0'

let parse_bank s =
  s |>
  String.to_seq |>
  Seq.map int_of_char |>
  List.of_seq

let banks =
  Input.get ~year:2025 ~day:3 |>
  String.split_on_char '\n' |>
  List.map parse_bank

let rec max_voltage = function
  | a :: b :: xs ->
      let biggest_ones_digit =
        List.fold_left max b xs in
      let using_a = 10 * a + biggest_ones_digit in
      max using_a (max_voltage (b :: xs))
  | _ -> 0

let sum_maxes banks =
  banks |>
  List.map max_voltage |>
  List.fold_left ( + ) 0

let () =
  let big = sum_maxes banks in
  Printf.printf "Part1: %d\n" big
