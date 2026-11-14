open! Core

type t =
  { letters : (Point_2d.t * char) list
  ; num_rows : int
  ; num_cols : int
  }

let of_string s =
  let rows = String.split_lines s in
  let num_rows = List.length rows in
  let num_cols =
    match rows with
    | x :: _ -> String.length x
    | [] -> 0
  in
  let letters =
    rows
    |> List.concat_mapi ~f:(fun row line ->
      String.to_list line
      |> List.mapi ~f:(fun col letter -> Point_2d.create ~row ~col, letter))
  in
  { letters; num_rows; num_cols }
;;

let where_letter_is { letters; _ } ~letter =
  List.filter letters ~f:(fun (_, this_letter) -> Char.equal this_letter letter)
  |> List.map ~f:fst
;;

let to_list { letters; _ } = letters
let num_rows { num_rows; _ } = num_rows
let num_cols { num_cols; _ } = num_cols
