let input = Aoc.Input.get_input 7

let parse input =
  input |> String.trim |> String.split_on_char '\n' |>
  List.map (fun line ->
    let splits = String.split_on_char ':' line in
    let target = int_of_string (List.nth splits 0) and
    vals = List.nth splits 1 |> String.trim |> String.split_on_char ' ' |> List.map (int_of_string) in
    (target, vals)
  )

let can_make item =
  let (target, vals) = item in
  let rec aux acc lst =
    match lst with
      | [] -> 
          acc = target
      | elt :: rest -> 
          if aux (acc + elt) rest || aux (acc * elt) rest then true
          else false
  in match vals with
    | [] -> failwith "Bad input"
    | x :: xs -> aux x xs

let concat_int a b =
  (string_of_int a) ^ (string_of_int b) |> int_of_string

let can_make2 item =
  let (target, vals) = item in
  let rec aux acc lst =
    match lst with
      | [] -> 
          acc = target
      | elt :: rest -> 
          aux (acc + elt) rest || aux (acc * elt) rest || aux (concat_int acc elt) rest
  in match vals with
    | [] -> failwith "Bad input"
    | x :: xs -> aux x xs


let part1 =
  input |> parse |> List.filter can_make |> List.fold_left (fun acc (target, _) ->
    acc + target
  ) 0

let part2 =
  input |> parse |> List.filter can_make2 |> List.fold_left (fun acc (target, _) ->
    acc + target
  ) 0

let () =
  Printf.printf "%d, %d" part1 part2
