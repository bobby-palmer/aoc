(** list[string] -> (list[string], list[string]) spliting and not inlcuding the
    first string that is empty*)
let split_on_empty lines = 
  let rec aux acc = function 
    | x :: xs when String.length x = 0 -> 
        (List.rev acc, xs) 
    | x :: xs -> 
        aux (x :: acc) xs 
    | _ -> 
        failwith "Bad input" in 
  aux [] lines

let parse_range range = Scanf.sscanf range "%d-%d" (fun a b -> (a, b))

let parse_query = int_of_string

let parse input =
  let (ranges, queries) = input
  |> String.trim
  |> String.split_on_char '\n'
  |> split_on_empty in
  (List.map parse_range ranges, List.map parse_query queries)

let contains id (l, r) =
  l <= id && id <= r

let part1 input =
  let (ranges, queries) = parse input in
  queries
  |> List.filter (fun q -> List.exists (contains q) ranges)
  |> List.length

(** Insert interval into list in sorted order, merging adjacent intervals *)
let rec coalesse lst (l, r) =
  match lst with
  | (u, v) :: xs ->
      if r + 1 < u then 
        (l, r) :: ((u, v) :: xs)
      else if v + 1 < l then 
        (u, v) :: coalesse xs (l, r)
      else 
        coalesse xs (min l u, max r v)
  | [] -> [(l, r)]

let part2 input =
  let (ranges, _) = parse input in
  ranges
  |> List.fold_left (coalesse) []
  |> List.map (fun (l, r) -> r - l + 1)
  |> List.fold_left ( + ) 0


let () =
  let input = Aoc.Input.get_input 5 in
  input |> part1 |> print_int;
  print_newline ();
  input |> part2 |> print_int;
  print_newline();
;
