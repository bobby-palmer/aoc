open Aoc

type direction = North | South | East | West

let direction_of_char = function
  | '<' -> West
  | '>' -> East
  | '^' -> North
  | 'v' -> South
  | _ -> failwith "Bad input"

let move_in_direction (x, y) = function
  | North -> (x, y + 1)
  | South -> (x, y - 1)
  | West -> (x - 1, y)
  | East -> (x + 1, y)

let parse input =
  String.to_seq input 
  |> Seq.map direction_of_char
  |> List.of_seq

let simulate_directions parsed =
  let (final_position, earlier_positions) =
    parsed 
    |> List.fold_left_map (fun position direction ->
          let next_position = move_in_direction position direction in
          (next_position, position)
        ) (0, 0) 
  in
  final_position :: earlier_positions


let part1 parsed =
  parsed
  |> simulate_directions
  |> List.sort_uniq compare
  |> List.length

let is_even num = num mod 2 = 0

let part2 parsed =
  let santa =
    parsed
    |> List.filteri (fun idx _ -> is_even idx)
    |> simulate_directions
  and robo =
    parsed
    |> List.filteri (fun idx _ -> not (is_even idx))
    |> simulate_directions
  in
  List.append santa robo
  |> List.sort_uniq compare
  |> List.length

let () =
  let parsed = Input.get 2015 3 |> parse in
  part1 parsed |> print_int;
  print_newline ();
  part2 parsed |> print_int;
  print_newline ()
