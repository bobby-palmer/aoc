open! Aoc
open! Core

let input = Input.get ~year:2025 ~day:6 |> Or_error.ok_exn

(* Assumes a rectangle input *)
let rec transpose matrix =
  let row, rest =
    List.fold_map matrix ~init:[] ~f:(fun acc row ->
      match row with
      | x :: xs -> x :: acc, xs
      | [] as xs -> acc, xs)
  in
  match row with
  | [] -> []
  | row -> row :: transpose rest
;;

let part1 input =
  let split_space s = String.split s ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
  let eqns = String.split_lines input |> List.map ~f:split_space |> transpose in
  let apply operator nums =
    let nums = List.map nums ~f:Int.of_string in
    match operator with
    | "*" -> List.fold nums ~init:1 ~f:( * )
    | "+" -> List.fold nums ~init:0 ~f:( + )
    | _ -> raise_s [%message "invalid_operator" (operator : string)]
  in
  List.map eqns ~f:(fun eqn ->
    match eqn with
    | operator :: nums -> apply operator nums
    | _ -> raise_s [%message "expected an operator and some nums for each row"])
  |> Util.sum_list
;;

let _part2 _input =
  raise_s [%message "this one super annoying and I dont wanna implement rn"]
;;

let () = printf "Part1: %d\n" (part1 input)
