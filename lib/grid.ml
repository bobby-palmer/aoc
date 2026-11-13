open! Core

let of_string s =
  String.strip s
  |> String.split_lines
  |> List.concat_mapi ~f:(fun row line ->
    String.to_list line
    |> List.mapi ~f:(fun col letter -> Point_2d.create ~row ~col, letter))
  |> Point_2d.Map.of_alist_exn
;;
