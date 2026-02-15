open Aoc

let input = Input.get 2020 1
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map int_of_string

let rec two_sum sum = function
  | x :: xs ->
      ( match List.find_opt (fun y -> x + y = sum) xs with
      | Some y -> Some (x, y)
      | None -> two_sum sum xs)
  | [] -> None

let part1 lst =
  match two_sum 2020 lst with
  | Some (x, y) -> x * y
  | None -> failwith "No match found"

let () = part1 input |> Printf.printf "Part1: %d\n"

let rec part2 = function
  | x :: xs ->
      (match two_sum (2020 - x) xs with
      | Some (y, z) -> x * y * z
      | None -> part2 xs)
  | [] -> failwith "No match found"

let () = part2 input |> Printf.printf "Part2: %d\n"
