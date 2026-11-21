open! Core
open! Aoc

type policy =
  { lowest : int
  ; highest : int
  ; letter : char
  }

let policies_and_passwords =
  let parse_line s =
    Scanf.sscanf s "%d-%d %c: %s" (fun lowest highest letter password ->
      { lowest; highest; letter }, password)
  in
  Input.get_exn ~year:2020 ~day:2 |> String.split_lines |> List.map ~f:parse_line
;;

let is_valid ({ lowest; highest; letter }, password) =
  let count = String.count password ~f:(Char.equal letter) in
  lowest <= count && count <= highest
;;

let part1 = List.count policies_and_passwords ~f:is_valid

let is_valid ({ lowest; highest; letter }, password) =
  let matches index = Char.equal password.[index - 1] letter in
  match matches lowest, matches highest with
  | true, true | false, false -> false
  | _, _ -> true
;;

let part2 = List.count policies_and_passwords ~f:is_valid

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;

