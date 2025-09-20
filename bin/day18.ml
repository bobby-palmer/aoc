open Aoc

let input = Input.get_input 18

let parse input = 
  let parse_line line =
    match line |> String.split_on_char ',' with
      | a :: b :: [] -> (int_of_string a, int_of_string b)
      | _ -> failwith "Bad line"
  in
  input |> String.trim |> String.split_on_char '\n' |> List.map (parse_line)

let size = 71

module PairOrd = struct
  type t = int * int
  let compare = compare
end

module PairSet = Set.Make(PairOrd)

let is_in_grid (r, c) =
  r >= 0 && r < size && c >= 0 && c < size

type direction = Up | Down | Left | Right

let move_in_dir (r, c) = function
  | Up -> (r - 1, c)
  | Down -> (r + 1, c)
  | Left -> (r, c - 1)
  | Right -> (r, c + 1)

let get_dist (sr, sc) (er, ec) dont_visit =
  let rec aux acc to_visit dont_visit =
    if to_visit |> List.find_opt ((=) (er, ec)) |> Option.is_some then acc
    else if to_visit |> List.length = 0 then failwith "Cannot reach end"
    else (
      let (to_visit, dont_visit) = 
        to_visit |> List.map (fun elt -> [Up; Down; Left; Right] |> List.map (move_in_dir elt))
          |> List.concat |> List.fold_left (fun (acc, dont_visit) elt -> 
                if is_in_grid elt && not (PairSet.mem elt dont_visit) then
                  (elt :: acc, dont_visit |> PairSet.add elt)
                else
                  (acc, dont_visit)
          ) ([], dont_visit)
      in
      aux (acc + 1) to_visit dont_visit
    )
  in
  aux 0 ([(sr, sc)]) dont_visit

let rec list_clamp len lst =
  if len = 0 then []
  else match lst with
    | x :: xs ->
        x:: list_clamp (len - 1) xs
    | [] -> []

let part1 input =
  input |> parse |> list_clamp 1024 |> PairSet.of_list |> get_dist (0, 0) (size - 1, size - 1)

let () = print_int (part1 input)
let () = print_newline ()

let rec test_list set lst =
  match lst with
    | x :: xs -> (
        try 
          let set = set |> PairSet.add x in
          let _ = get_dist (0, 0) (size - 1, size - 1) set in
          test_list set xs
        with _ -> x
      )
    | _ -> failwith "Bad input"

let part2 input =
  input |> parse |> test_list PairSet.empty

let () =
  let (x, y) = part2 input in
  Printf.printf "%d, %d\n" x y
