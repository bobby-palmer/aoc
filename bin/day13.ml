open Aoc

let input = Input.get_input 13

let string_not_empty str = String.length str > 0

let extract_int_pair s =
  Scanf.sscanf s "%[^0-9]%d%[^0-9]%d%[^0-9]" (fun _ a _ b _ -> (a, b))

let parse input =
  input |> String.trim |> String.split_on_char '\n' 
    |> List.filter (string_not_empty) |> List.map (extract_int_pair)

let range a b =
  let aux count =
    if count < b then Some (count, count + 1) else None
  in
  Seq.unfold (aux) a

let min_opt a b =
  match (a, b) with
    | (Some a, Some b) -> Some (min a b)
    | (Some a, None) -> Some a
    | (None, Some b) -> Some b
    | (None, None) -> None

let play_machine a b goal =
  let (ax, ay) = a and (bx, by) = b and (gx, gy) = goal in
  let maybe_cost = range 0 101 |> Seq.map (fun mult_a ->
    let (dx, dy) = (gx - ax * mult_a, gy - ay * mult_a) in
    if dx >= 0 && dx mod bx = 0 && (dx / bx) * by = dy then Some (3 * mult_a + dx / bx)
    else None
  ) |> Seq.fold_left (min_opt) None in 
  match maybe_cost with
    | Some cost -> cost
    | None -> 0

let rec play_machines = function
  | a :: b :: goal :: rest -> (play_machine a b goal) :: play_machines rest
  | [] -> []
  | _ -> failwith "Bad input"

let part1 input =
  input |> parse |> play_machines |> List.fold_left (+) 0

let () = Printf.printf "%d" (part1 input)
