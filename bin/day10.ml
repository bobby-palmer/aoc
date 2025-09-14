let input = Aoc.Input.get_input 10

let int_of_digit ch = int_of_char ch - int_of_char '0'

let parse input = 
  let tbl = Hashtbl.create 0 in
  input |> String.trim |> String.split_on_char '\n' |> List.iteri (fun row str ->
    str |> String.iteri (fun col digit ->
      Hashtbl.replace tbl (row, col) (int_of_digit digit)
    )
  );
  tbl

module PairOrd = struct 
  type t = (int * int)
  let compare = compare
end

module PairSet = Set.Make(PairOrd)

let count_unique lst =
  PairSet.empty |> PairSet.add_seq (List.to_seq lst) |> PairSet.cardinal

let part1 input =
  let input = parse input in
  let rec aux row col target =
    if Hashtbl.find_opt input (row, col) <> Some target then []
    else if target = 9 then [(row, col)]
    else 
      [(-1, 0); (1, 0); (0, -1); (0, 1)] |> List.map (fun (dr, dc) ->
        aux (row + dr) (col + dc) (target + 1)
      ) |> List.concat
    
  in
  input |> Hashtbl.to_seq_keys |> Seq.fold_left (fun acc (row, col) ->
    acc + count_unique (aux row col 0)
  ) 0 

let part2 input =
  let input = parse input in
  let rec aux row col target =
    if Hashtbl.find_opt input (row, col) <> Some target then 0
    else if target = 9 then 1
    else 
      [(-1, 0); (1, 0); (0, -1); (0, 1)] |> List.fold_left (fun acc (dr, dc) ->
        acc + aux (row + dr) (col + dc) (target + 1)
      ) 0
  in
  input |> Hashtbl.to_seq_keys |> Seq.fold_left (fun acc (row, col) -> acc + aux row col 0) 0


let () =
  Printf.printf "%d, %d" (part1 input) (part2 input)
