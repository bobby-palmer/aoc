open Aoc

let input = Input.get_input 19

let parse input =
  let lines = input |> String.trim |> String.split_on_char '\n' in
  match lines with
    | x :: _ :: xs -> 
        let patterns = x |> String.split_on_char ',' |> List.map (String.trim) in
        (patterns, xs)
    | _ -> failwith "Bad input"

let ways_to_make patterns goal =
  let tbl = Hashtbl.create 0 in
  let rec aux index =
    if index = String.length goal then 1
    else match Hashtbl.find_opt tbl index with
      | Some v -> v
      | None ->
          let v = patterns |> List.map (fun prefix ->
            if index + String.length prefix > String.length goal then 0
            else (
              let sub = String.sub goal index (String.length prefix) in
              if sub = prefix then
                aux (index + String.length prefix)
              else 0
            )
          ) |> List.fold_left (+) 0 in
          Hashtbl.replace tbl index v;
          v
  in
  aux 0

let part1 input = 
  let (patterns, goals) = parse input in
  goals |> List.map (ways_to_make patterns) 
      |> List.filter ((<) 0) |> List.length

let () = print_int (part1 input)
let () = print_newline ()

let part2 input =
  let (patterns, goals) = parse input in
  goals |> List.map (ways_to_make patterns) |> List.fold_left (+) 0

let () = print_int (part2 input)
let () = print_newline ()
