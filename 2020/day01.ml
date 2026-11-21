open! Aoc
open! Core

let expense_report =
  Input.get_exn ~year:2020 ~day:1 |> String.split_lines |> List.map ~f:Int.of_string
;;

let rec find_n_sum nums ~sum ~n =
  match n, nums with
  | 1, _ -> List.find nums ~f:(fun num -> num = sum) |> Option.map ~f:List.singleton
  | _, [] -> None
  | _, x :: xs ->
    (match find_n_sum xs ~sum:(sum - x) ~n:(n - 1) with
     | Some rest -> Some (x :: rest)
     | None -> find_n_sum xs ~sum ~n)
;;

let part1 =
  find_n_sum expense_report ~sum:2020 ~n:2
  |> Option.value_exn
  |> List.fold ~init:1 ~f:Int.( * )
;;

let part2 =
  find_n_sum expense_report ~sum:2020 ~n:3
  |> Option.value_exn
  |> List.fold ~init:1 ~f:Int.( * )
;;

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;
