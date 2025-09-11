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


let solve input =
  let input = parse input in
  let p1 = input |> List.filter can_make |> List.fold_left (fun acc (target, _) ->
    acc + target
  ) 0 in
  (p1, p1)
