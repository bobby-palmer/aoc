open! Aoc
open! Core

type t =
  { start : Point_2d.t
  ; splitters : Point_2d.Set.t
  }

let parse input =
  let grid = Grid.of_string input in
  let start = Map.filter grid ~f:(Char.( = ) 'S') |> Map.keys |> List.hd_exn in
  let splitters = Map.filter grid ~f:(Char.( = ) '^') |> Map.key_set in
  { start; splitters }
;;

let part1 { start; splitters } =
  let beams = Set.add Point_2d.Set.empty start in
  let last_split_row =
    Set.fold splitters ~init:0 ~f:(fun acc splitter -> max acc (Point_2d.row splitter))
  in
  let rec run beams =
    let split, unaffected =
      Point_2d.Set.map beams ~f:Point_2d.south |> Set.partition_tf ~f:(Set.mem splitters)
    in
    let post_split =
      let left = Point_2d.Set.map split ~f:Point_2d.west in
      let right = Point_2d.Set.map split ~f:Point_2d.east in
      Set.union left right
    in
    let beams = Set.union unaffected post_split in
    let this_splits = Set.length split in
    let is_done = Set.for_all beams ~f:(fun beam -> Point_2d.row beam > last_split_row) in
    match is_done with
    | true -> this_splits
    | false -> this_splits + run beams
  in
  run beams
;;

let input = Input.get ~year:2025 ~day:7 |> Or_error.ok_exn |> parse
let () = printf "Part1: %d\n" (part1 input)
