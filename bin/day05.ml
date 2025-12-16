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

let () =
  let input = Aoc.Input.get_input 5 in
  input |> part1 |> print_int;
  print_newline ();;
