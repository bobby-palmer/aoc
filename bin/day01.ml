type intruction = Left of int | Right of int

let instruction_of_string str =
  let direction, distance = Scanf.sscanf str "%c%d" (fun a b -> a, b) in
  match direction with
    | 'L' -> Left distance
    | 'R' -> Right distance
    | _ -> failwith "Bad input"

let parse input =
  input
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map instruction_of_string

let do_moves instructions =
  instructions
    |> List.fold_left_map (fun pos instruction ->
          match instruction with
          | Left amount -> (pos - amount, pos - amount)
          | Right amount -> (pos + amount, pos + amount)
        ) 50
    |> snd

let part1 input =
  input
    |> parse
    |> do_moves
    |> List.map (fun pos -> 
          if pos mod 100 = 0 then 1
          else 0
        )
    |> List.fold_left (+) 0

let math_floor_div a b =
  let q = a / b and
  r = a mod b in
  (* Adjusts the result only for negative dividends with non-zero remainder *)
  if r <> 0 && a < 0 then q - 1 
  else q

(** Count how many times i % 100 = 0 in (a, b] *)
let count_zeroes a b =
  let upper = max a b in
  let lower = min a b in
  (* Count of I in [lower, upper] such that I % 100 = 0 *)
  let inclusive = (math_floor_div upper 100) - (math_floor_div (lower - 1) 100) in
  if a mod 100 = 0 then inclusive - 1
  else inclusive

let part2 input =
  input
    |> parse
    |> do_moves
    |> List.fold_left_map (fun last current ->
          (current, count_zeroes last current) 
        ) 50
    |> snd
    |> List.fold_left (+) 0

let () = 
  let input = Aoc.Input.get_input 1 in
  part1 input |> print_int;
  print_newline ();
  part2 input |> print_int;
  print_newline ();
;
