open! Core
open! Aoc

let tiles =
  let parse line = Scanf.sscanf line "%d,%d" (fun row col -> Point_2d.create ~row ~col) in
  Input.get ~year:2025 ~day:9
  |> Or_error.ok_exn
  |> String.split_lines
  |> List.map ~f:parse
;;

module Rectangle = struct
  type t =
    { min_row : int
    ; max_row : int
    ; min_col : int
    ; max_col : int
    }

  let of_corners p1 p2 =
    let min_row, max_row = Util.min_max (Point_2d.row p1) (Point_2d.row p2) ~compare in
    let min_col, max_col = Util.min_max (Point_2d.col p1) (Point_2d.col p2) ~compare in
    { min_row; max_row; min_col; max_col }
  ;;

  let area { min_row; max_row; min_col; max_col } =
    (max_col - min_col + 1) * (max_row - min_row + 1)
  ;;

  let section { min_row; max_row; min_col; max_col } point =
    let row = Point_2d.row point in
    let col = Point_2d.col point in
    let row_section =
      match row <= min_row, row >= max_row with
      | true, _ -> 0
      | _, true -> 2
      | _, _ -> 1
    in
    let col_section =
      match col <= min_col, col >= max_col with
      | true, _ -> 0
      | _, true -> 2
      | _, _ -> 1
    in
    row_section, col_section
  ;;
end

let part1 =
  Util.pairs_of_list tiles
  |> Sequence.map ~f:(fun (p1, p2) -> Rectangle.of_corners p1 p2 |> Rectangle.area)
  |> Sequence.fold ~init:0 ~f:max
;;

let rec list_windows = function
  | a :: b :: rest -> (a, b) :: list_windows (b :: rest)
  | _ -> []
;;

let part2 =
  let segments_to_check = list_windows (tiles @ [ List.hd_exn tiles ]) in
  let segment_is_good rect (p1, p2) =
    match Rectangle.section rect p1, Rectangle.section rect p2 with
    | (1, x), (1, y) -> x = y
    | (x, 1), (y, 1) -> x = y
    | _, _ -> true
  in
  let all_segments_good rect = List.for_all segments_to_check ~f:(segment_is_good rect) in
  Util.pairs_of_list tiles
  |> Sequence.map ~f:(fun (p1, p2) -> Rectangle.of_corners p1 p2)
  |> Sequence.filter ~f:all_segments_good
  |> Sequence.map ~f:Rectangle.area
  |> Sequence.fold ~init:0 ~f:max
;;

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;

