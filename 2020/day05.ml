open! Aoc
open! Core

module Boarding_pass = struct
  module Direction = struct
    type t =
      | Left
      | Right
      | Front
      | Back

    let of_char = function
      | 'L' -> Left
      | 'R' -> Right
      | 'F' -> Front
      | 'B' -> Back
      | letter -> raise_s [%message "invalid letter in input" (letter : char)]
    ;;
  end

  let of_string s = String.to_list s |> List.map ~f:Direction.of_char
end

module Seat_range = struct
  module Range = struct
    type t = int * int

    type half =
      | Lower
      | Upper

    let get_half (lower, upper) ~half =
      let mid = (lower + upper) / 2 in
      match half with
      | Lower -> lower, mid
      | Upper -> mid, upper
    ;;

    let resolved (lower, upper) =
      match lower + 1 = upper with
      | true -> Ok lower
      | false ->
        Or_error.error_s
          [%message "range is not resolved to a value" (lower : int) (upper : int)]
    ;;
  end

  type t =
    { row_range : Range.t
    ; col_range : Range.t
    }

  let init = { row_range = 0, 128; col_range = 0, 8 }
  let map_row t ~f = { t with row_range = f t.row_range }
  let map_col t ~f = { t with col_range = f t.col_range }

  let seat_id { row_range; col_range } =
    let open Or_error.Let_syntax in
    let%bind row = Range.resolved row_range in
    let%map col = Range.resolved col_range in
    (8 * row) + col
  ;;

  let apply_direction t = function
    | Boarding_pass.Direction.Left -> map_col t ~f:(Range.get_half ~half:Lower)
    | Right -> map_col t ~f:(Range.get_half ~half:Upper)
    | Front -> map_row t ~f:(Range.get_half ~half:Lower)
    | Back -> map_row t ~f:(Range.get_half ~half:Upper)
  ;;

  let apply_pass = List.fold ~f:apply_direction
end

let passes =
  Input.get_exn ~year:2020 ~day:5
  |> String.split_lines
  |> List.map ~f:Boarding_pass.of_string
;;

let seat_ids =
  List.map passes ~f:(fun pass ->
    Seat_range.apply_pass pass ~init:Seat_range.init
    |> Seat_range.seat_id
    |> Or_error.ok_exn)
;;

let part1 = List.max_elt seat_ids ~compare |> Option.value_exn

let part2 =
  let rec aux = function
    | a :: b :: _ when a + 1 = b - 1 -> Some (a + 1)
    | _ :: xs -> aux xs
    | [] -> None
  in
  List.sort seat_ids ~compare |> aux |> Option.value_exn
;;

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;
