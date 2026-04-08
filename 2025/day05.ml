open Aoc

(** Split list on first element where p x returns true (removes x) *)
let list_split_on p lst =
  let rec aux acc = function
    | x :: xs ->
        if p x then (List.rev acc, xs)
        else aux (x :: acc) xs
    | [] -> failwith "no matching elements"
  in aux [] lst

let string_empty s = String.length s = 0

let parse_range s = Scanf.sscanf s "%d-%d" (fun a b -> (a, b))

let (ingredient_ranges, availible_ingredients) =
  let (ranges, availible) = 
    Input.get ~year:2025 ~day:5
    |> String.split_on_char '\n'
    |> list_split_on string_empty
  in
  (
    ranges |> List.map parse_range,
    availible |> List.map int_of_string
  )

let is_fresh ingredient =
  List.exists (fun (a, b) -> 
    a <= ingredient && ingredient <= b
  ) ingredient_ranges

let () =
  let count_fresh = availible_ingredients
    |> List.filter is_fresh 
    |> List.length 
  in
  Printf.printf "Part1: %d\n" count_fresh

let rec insert_range (a, b) = function
  | (x, y) :: xs when a > y + 1 ->
      (x, y) :: insert_range (a, b) xs
  | (x, y) :: xs when b < x - 1 ->
      (a, b) :: (x, y) :: xs
  | (x, y) :: xs ->
      insert_range (min a x, max b y) xs
  | [] -> [(a, b)]

let count_in_ranges =
  List.fold_left (fun acc (a, b) ->
    acc + (b - a + 1)
  ) 0

let () =
  let num_fresh = ingredient_ranges
    |> List.fold_left (fun acc range -> insert_range range acc) []
    |> count_in_ranges
  in
  Printf.printf "Part2: %d\n" num_fresh
