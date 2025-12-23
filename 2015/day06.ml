open Aoc

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

let contains (x1, y1) (x2, y2) (x, y) =
  (min x1 x2 <= x && x <= max x1 x2) &&
  (min y1 y2 <= y && y <= max y1 y2)

(* Make a sequence over [0, limit) *)
let iota limit =
  Seq.unfold (fun current ->
    if current < limit then Some (current, current + 1)
    else None
  ) 0

(* Make a sequence over all (x, y) for lights *)
let make_grid () =
  let size = 1000 in
  iota size
  |> Seq.flat_map (fun x ->
    iota size |> Seq.map (fun y -> (x, y))
  )

let simulate lst =
  let rec aux = function
  | On :: _ -> true
  | Off :: _ -> false
  | Toggle :: xs -> not (aux xs)
  | [] -> false in
  aux (List.rev lst)

let part1 instructions =
  make_grid ()
  |> Seq.map (fun square ->
    instructions
    |> List.filter_map (fun (i, p1, p2) ->
      if contains p1 p2 square then Some i
      else None
    )
    |> simulate
  )
  |> Seq.filter (fun b -> b)
  |> Seq.length

let () =
  let instructions = parse_instructions @@ Input.get 2015 6 in
  part1 instructions |> print_int
