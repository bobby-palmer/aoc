open Aoc

let input = Input.get_input 13

let string_not_empty str = String.length str > 0

let extract_int_pair s =
  Scanf.sscanf s "%[^0-9]%d%[^0-9]%d%[^0-9]" (fun _ a _ b _ -> (a, b))

let parse input =
  input |> String.trim |> String.split_on_char '\n' 
    |> List.filter (string_not_empty) |> List.map (extract_int_pair)

exception Imprecision

let (/!) num denom =
  if num mod denom <> 0 then raise Imprecision
  else num / denom

(*
  RREF:
  | a1 b1 g1
  | a2 b2 g2
  | 0 (b1 a2 - b2 a1) (g1 a2 - g2 a1)

  To find x -> (g1 - b1 * y) / a1

  cases:
  | [0, l != 0, r != 0] -> one soln
  | [0, l == 0, r != 0] -> No soln
  | [0, l == 0, r == 0] -> Inf soln
 *)
let intersect (a1, a2) (b1, b2) (g1, g2) =
  let l = b1 * a2 - b2 * a1 and r = g1 * a2 - g2 * a1 in
  let y = r /! l in
  let x = (g1 - b1 * y) /! a1 in
  (x, y)

let play_machine (a1, a2) (b1, b2) (g1, g2) =
  try
    let (x, y) = intersect (a1, a2) (b1, b2) (g1, g2) in
    if x >= 0 && y >= 0 then 3 * x + y
    else 0
  with _ -> 0

let rec play_machines = function
  | a :: b :: goal :: rest -> (play_machine a b goal) :: play_machines rest
  | [] -> []
  | _ -> failwith "Bad input"

let part1 input =
  input |> parse |> play_machines |> List.fold_left (+) 0

let adjust_goal idx (elt1, elt2) =
  let adjustment = 10000000000000 in
  if idx mod 3 = 2 then (elt1 + adjustment, elt2 + adjustment)
  else (elt1, elt2)

let part2 input =
  input |> parse |> List.mapi (adjust_goal) |> play_machines |> List.fold_left (+) 0

let () = Printf.printf "%d, %d" (part1 input) (part2 input)
