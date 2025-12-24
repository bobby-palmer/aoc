open Aoc

let n = 1000

type instruction = On | Off | Toggle

let instruction_regex = {|\(turn on\|turn off\|toggle\)|}
let number_regex = {|\([0-9]+\)|}
let pair_regex = number_regex ^ "," ^ number_regex
let r = Str.regexp (
    "^" ^ instruction_regex ^ " " ^ pair_regex ^ " through " ^ pair_regex
  )

let parse_instruction line =
  let _ = Str.search_forward r line 0 in
  let instruction = 
    match Str.matched_group 1 line with
      | "turn on" -> On
      | "turn off" -> Off
      | "toggle" -> Toggle
      | _ -> failwith "Bad prefix" in
  let x1 = Str.matched_group 2 line |> int_of_string
  and y1 = Str.matched_group 3 line |> int_of_string 
  and x2 = Str.matched_group 4 line |> int_of_string 
  and y2 = Str.matched_group 5 line |> int_of_string in
  (instruction, (x1, y1), (x2, y2))

let parse_instructions input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse_instruction

(* Make seq of numbers in [a, b] *)
let make_range_inclusive a b =
  let lower = min a b
  and upper = max a b in
  Seq.unfold (fun current ->
    if current <= upper then Some (current, current + 1)
    else None
  ) lower

let points_in_rectangle (x1, y1) (x2, y2) =
  make_range_inclusive x1 x2
  |> Seq.flat_map (fun x ->
    make_range_inclusive y1 y2
    |> Seq.map (fun y -> (x, y))
  )

let part1 instructions =
  let grid = Array.make_matrix n n false in
  instructions 
  |> List.iter (fun (i, p1, p2) ->
    points_in_rectangle  p1 p2
    |> Seq.iter (fun (x, y) ->
      grid.(x).(y) <-
        match i with
          | On -> true
          | Off -> false
          | Toggle -> not (grid.(x).(y))
    )
  );
  Array.fold_left (
    Array.fold_left (fun acc b -> 
      if b then acc + 1
      else acc
    )
  ) 0 grid

let part2 instructions =
  let grid = Array.make_matrix n n 0 in
  instructions 
  |> List.iter (fun (i, p1, p2) ->
    points_in_rectangle p1 p2
    |> Seq.iter (fun (x, y) ->
      let old = grid.(x).(y) in
      grid.(x).(y) <-
        match i with
          | On -> old + 1
          | Off -> max (old - 1) 0
          | Toggle -> old + 2
    )
  );
  grid 
  |> Array.fold_left (Array.fold_left (+)) 0

let () =
  let instructions = parse_instructions @@ Input.get 2015 6 in
  part1 instructions |> print_int;
  print_newline ();
  part2 instructions |> print_int;
  print_newline ()
