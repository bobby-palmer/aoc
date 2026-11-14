open! Aoc
open! Core

let parse_rolls input =
  Grid.of_string input |> Grid.where_letter_is ~letter:'@' |> Point_2d.Set.of_list
;;

let adjacent_rolls roll ~rolls =
  Point_2d.extended_neighbors roll |> List.count ~f:(Set.mem rolls)
;;

let remove_rolls rolls =
  let removed, remaining =
    Set.partition_tf rolls ~f:(fun roll -> adjacent_rolls roll ~rolls < 4)
  in
  ~removed, ~remaining
;;

let part1 rolls =
  let ~removed, ~remaining:_ = remove_rolls rolls in
  Set.length removed
;;

let rec part2 rolls =
  let ~removed, ~remaining = remove_rolls rolls in
  match Set.length removed with
  | 0 -> 0
  | num_removed -> num_removed + part2 remaining
;;

let rolls = Input.get ~year:2025 ~day:4 |> Or_error.ok_exn |> parse_rolls

let () =
  printf "Part1: %d\n" (part1 rolls);
  printf "Part2: %d\n" (part2 rolls)
;;
