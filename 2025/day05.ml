open! Aoc
open! Core

let parse_range r = Scanf.sscanf r "%d-%d" (fun a b -> a, b)

let parse input =
  match
    String.split_lines input
    |> List.group ~break:(fun a b -> String.is_empty a || String.is_empty b)
    |> List.filter ~f:(Fn.non (List.for_all ~f:String.is_empty))
  with
  | [ id_ranges; availible_ids ] ->
    let id_ranges = List.map id_ranges ~f:parse_range in
    let availible_ids = List.map availible_ids ~f:Int.of_string in
    id_ranges, availible_ids
  | something_else ->
    raise_s
      [%message "expected exactly two sections" ~got:(something_else : string list list)]
;;

let is_in_range ~range:(lower, higher) num = lower <= num && num <= higher

let part1 (id_ranges, ids) =
  List.count ids ~f:(fun id ->
    List.exists id_ranges ~f:(fun range -> is_in_range ~range id))
;;

let ranges_overlap (x, y) (u, v) =
  let ranges_dont_overlap = v < x || y < u in
  not ranges_dont_overlap
;;

let insert_range ranges range =
  let to_join, disjoint = List.partition_tf ranges ~f:(ranges_overlap range) in
  let range = List.fold to_join ~init:range ~f:(fun (x, y) (u, v) -> min x u, max y v) in
  range :: disjoint
;;

let count_range (x, y) = y - x + 1

let part2 (id_ranges, _) =
  List.fold id_ranges ~init:[] ~f:insert_range |> List.map ~f:count_range |> Util.sum_list
;;

let input = Input.get ~year:2025 ~day:5 |> Or_error.ok_exn |> parse

let () =
  printf "Part1: %d\n" (part1 input);
  printf "Part2: %d\n" (part2 input)
;;
