let input = 
  In_channel.input_all In_channel.stdin |>
  String.trim |>
  String.split_on_char '\n'

let split_input =
  let rec aux acc = function
    | [] -> (List.rev acc, [])
    | x :: xs ->
        if String.length x == 0 then (List.rev acc, xs)
        else aux (x :: acc) xs
  in
  aux [] input

let parse_edges lst =
  List.map (fun x -> Scanf.sscanf x "%d|%d" (fun a b -> (a, b))) lst

let parse_path lst =
  List.map (fun x -> String.split_on_char ',' x |> List.map int_of_string) lst

let (edges, paths) =
  split_input |> function
    | edges, paths -> (parse_edges edges, parse_path paths)

let should_be_before a b =
  edges |> List.exists (fun (before, after) -> a == before && b == after)

let is_before x xs =
  xs |> List.for_all (fun y -> should_be_before x y)

let rec is_ordered = function
  | [] -> true
  | x :: xs ->
      is_before x xs && is_ordered xs

let list_middle lst =
  let len = List.length lst in
  List.nth lst (len / 2)

let part1 =
  paths |> List.filter is_ordered |> List.map list_middle |> List.fold_left ( + ) 0

let is_unordered path = is_ordered path |> not

let sort_path = List.sort (fun a b -> if should_be_before a b then -1 else 1)

let part2 =
  paths |> List.filter is_unordered |> List.map sort_path |> List.map list_middle |> List.fold_left (+) 0

let () =
  Printf.printf "P1: %d" part1;
  Printf.printf "P2: %d" part2
