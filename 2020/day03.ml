open! Aoc
open! Core

type t =
  { rows : int
  ; cols : int
  ; trees : Point_2d.Set.t
  }

let map =
  let grid = Input.get_exn ~year:2020 ~day:3 |> Grid.of_string in
  let rows = Grid.num_rows grid in
  let cols = Grid.num_cols grid in
  let trees = Grid.where_letter_is grid ~letter:'#' |> Point_2d.Set.of_list in
  { rows; cols; trees }
;;

let simulate { rows; cols; trees } ~down ~right =
  let init = Point_2d.create ~row:0 ~col:0 in
  let next position =
    position
    |> Fn.apply_n_times ~n:down Point_2d.south
    |> Fn.apply_n_times ~n:right Point_2d.east
    |> Point_2d.map_col ~f:(fun col -> col % cols)
  in
  let positions =
    Sequence.unfold ~init ~f:(fun position -> Some (position, next position))
    |> Sequence.take_while ~f:(fun position -> Point_2d.row position < rows)
  in
  Sequence.count positions ~f:(Set.mem trees)
;;

let part1 = simulate map ~right:3 ~down:1

let part2 =
  [ 1, 1; 3, 1; 5, 1; 7, 1; 1, 2 ]
  |> List.map ~f:(fun (right, down) -> simulate map ~right ~down)
  |> List.fold ~init:1 ~f:Int.( * )
;;

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;
