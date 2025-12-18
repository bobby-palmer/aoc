let parse_line line =
  Scanf.sscanf line "%d,%d,%d" (fun x y z -> (x, y, z))

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse_line

(** Euclidian distance between points *)
let distance (a, b, c) (x, y, z) =
  let square num = num * num in
  sqrt @@ float_of_int (square (a - x) + square (b - y) + square (c - z))

let cmp_distance (a, b) (c, d) =
  compare (distance a b) (distance  c d)

(** Make a list of all 2 item pairs in the input list *)
let rec make_pairs = function
  | x :: xs ->
      let pairs_using_x = List.map (fun y -> (x, y)) xs
      and pairs_after_x = make_pairs xs in
      pairs_using_x @ pairs_after_x
  | [] -> []

(** Union find algo to get base parent for element *)
let rec get_parent tbl elt =
  match Hashtbl.find_opt tbl elt with
  | Some parent when parent != elt ->
      let parent = get_parent tbl parent in
      Hashtbl.replace tbl elt parent;
      parent
  | _ -> elt

let union tbl (a, b) =
  let a = get_parent tbl a
  and b = get_parent tbl b in
  Hashtbl.replace tbl a b

(** Return list of min(length, n) first elements *)
let rec list_take n = function
  | x :: xs when n > 0 -> x :: list_take (n - 1) xs
  | _ -> []

let part1 input =
  let boxes = parse input and
  tbl = Hashtbl.create 0 in
  boxes 
    |> make_pairs 
    |> List.sort cmp_distance 
    |> list_take 1000
    |> List.iter (union tbl);
  let counts = Hashtbl.create 0 in
  boxes
  |> List.map (get_parent tbl)
  |> List.iter (fun elt ->
        let count = match Hashtbl.find_opt counts elt with
        | Some count -> count + 1
        | None -> 1
        in Hashtbl.replace counts elt count
  );
  Hashtbl.to_seq_values counts
  |> List.of_seq
  |> List.sort compare
  |> List.rev
  |> list_take 3
  |> List.fold_left ( * ) 1

let () =
  let input = Aoc.Input.get_input 8 in
  input |> part1 |> print_int;
  print_newline ();
;
