open Aoc

let input = Input.get 2015 17

let containers = 
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map int_of_string

let rec ways_to_store amount = function
  | x :: xs ->
    ways_to_store amount xs + ways_to_store (amount - x) xs
  | [] ->
    if amount = 0 then 1
    else 0

let () = ways_to_store 150 containers |> print_int
let () = print_newline ()

let rec containers_to_store amount containers_used = function
  | x :: xs ->
    Seq.append 
      (containers_to_store amount containers_used xs)
      (containers_to_store (amount - x) (containers_used + 1) xs)
  | [] ->
    if amount = 0 then 
      Seq.cons (Some containers_used) Seq.empty
    else Seq.empty

let opt_min a b =
  match (a, b) with
  | (Some a, Some b) -> Some (min a b)
  | (Some a, None) -> Some a
  | (None, Some b) -> Some b
  | (None, None) -> None

let min_containers = containers_to_store 150 0 containers
  |> Seq.fold_left opt_min None
  |> Option.get

let ways_for_min_containers = containers_to_store 150 0 containers
  |> Seq.filter (fun count -> count = Some min_containers)
  |> Seq.length

let () = print_int ways_for_min_containers
let () = print_newline ()
