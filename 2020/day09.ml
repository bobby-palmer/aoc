open Aoc

let input =
  Input.get ~year:2020 ~day:9
    |> String.split_on_char '\n'
    |> List.map int_of_string

let rec can_make_2sum s = function
  | x :: xs -> 
      if List.mem (s - x) xs then true
      else can_make_2sum s xs
  | [] -> false

let rec find_invalid window = function
  | x :: xs ->
      if not (can_make_2sum x window) then x
      else
        let window = (List.tl window) @ [x] in
        find_invalid window xs
  | [] -> failwith "invalid input"

let list_split_n n lst =
  let rec aux acc = function
    | x :: xs when List.length acc < n ->
        aux (x :: acc) xs
    | xs -> (List.rev acc, xs)
  in
  aux [] lst

let part1 input =
  let (preamble, xs) = list_split_n 25 input in
  find_invalid preamble xs

let () = part1 input |> Printf.printf "Part1: %d\n"

let rec find_range_with_sum s lst =
  let rec aux acc lst =
    if List.fold_left ( + ) 0 acc = s then
      Some (List.rev acc)
    else match lst with
      | x :: xs ->
          aux (x :: acc) xs
      | [] -> None
  in match (aux [] lst) with
  | Some v -> v
  | None -> find_range_with_sum s (List.tl lst)

let () =
  let s = part1 input in
  let range = find_range_with_sum s input in
  let small = List.fold_left min Int.max_int range
  and big = List.fold_left max 0 range in
  Printf.printf "Part2: %d\n" (small + big)


