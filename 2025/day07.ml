open! Aoc
open! Core

type t =
  { start : Point_2d.t
  ; splitters : Point_2d.t list
  ; num_rows : int
  }

let parse input =
  let grid = Grid.of_string input in
  let start = Grid.where_letter_is grid ~letter:'S' |> List.hd_exn in
  let splitters = Grid.where_letter_is grid ~letter:'^' in
  let num_rows = Grid.num_rows grid in
  { start; splitters; num_rows }
;;

let run { start; splitters; num_rows } ~map_returned_to_cached =
  let cache = Point_2d.Table.create () in
  let rec aux =
    Hashtbl.find_and_call cache ~if_found:Fn.id ~if_not_found:(fun point ->
      match Point_2d.row point >= num_rows with
      | true -> 1
      | false ->
        let result =
          match Point_2d.south point with
          | next_point when List.mem splitters next_point ~equal:Point_2d.equal ->
            aux (Point_2d.west next_point) + aux (Point_2d.east next_point)
          | next_point -> aux next_point
        in
        Hashtbl.set cache ~key:point ~data:(map_returned_to_cached result);
        result)
  in
  aux start
;;

(* Kind of weird but wanted to share logic with part 2. We only cache 1 because
   we dont want to double count splits. And we subtract one to account for the
   starting beam which is not a split. *)
let part1 t = run t ~map_returned_to_cached:(fun _ -> 1) - 1
let part2 t = run t ~map_returned_to_cached:Fn.id
let input = Input.get ~year:2025 ~day:7 |> Or_error.ok_exn |> parse

let () =
  printf "Part1: %d\n" (part1 input);
  printf "Part2: %d\n" (part2 input)
;;
