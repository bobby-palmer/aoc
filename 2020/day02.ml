open Aoc

type password = {
  min_count: int;
  max_count: int;
  letter: char;
  password: string;
}

let password_of_string s =
  Scanf.sscanf s "%d-%d %c: %s" (fun min_count max_count letter password ->
    { min_count; max_count; letter; password }
  )

let is_verified p =
  let count = String.fold_left (fun acc letter ->
    if letter = p.letter then acc + 1
    else acc
  ) 0 p.password in
  p.min_count <= count && count <= p.max_count

let input = 
  Input.get ~year:2020 ~day:2
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map password_of_string

let part1 input =
  input
  |> List.filter is_verified
  |> List.length

let () = part1 input |> Printf.printf "Part1: %d\n"

let is_verified2 p =
  let a = p.password.[p.min_count - 1]
  and b = p.password.[p.max_count - 1] in
  (a = p.letter || b = p.letter) &&
  (a != b)

let part2 input =
  input
  |> List.filter is_verified2
  |> List.length

let () = part2 input |> Printf.printf "Part2: %d\n"
