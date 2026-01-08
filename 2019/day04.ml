(* Puzzle inputs given on the site *)
let lower = 264793
let upper = 803935

let first_digit num = num mod 10
let second_digit num = (num / 10) mod 10

let rec has_double_digit num =
  if num < 10 then false
  else if first_digit num = second_digit num then true
  else has_double_digit (num / 10)

let rec is_non_decreasing num =
  if num < 10 then true
  else if first_digit num < second_digit num then false
  else is_non_decreasing (num / 10)

let count_condition p =
  Seq.ints lower
    |> Seq.take_while (( >= ) upper)
    |> Seq.filter p
    |> Seq.length

let () =
  let count = count_condition (fun x -> 
    has_double_digit x && is_non_decreasing x
  ) in
  Printf.printf "part1: %d\n" count

(* List of the digits in integer (reverse order) *)
let rec list_of_int = function
  | x when x = 0 -> []
  | x -> (x mod 10) :: list_of_int (x / 10)

let has_exactly_double x =
  let digits = list_of_int x in
  let rec aux = function
    | a :: b :: c :: d :: _ when a <> b && b = c && c <> d -> true
    | a :: b :: c :: [] when a <> b && b = c -> true
    | _ :: xs -> aux xs
    | _ -> false 
  in
  match digits with
  (* Edge case: check if the first two are a double *)
  | a :: b :: c :: _ when a = b && b <> c -> true
  | lst -> aux lst

let () =
  let count = count_condition (fun x ->
    has_exactly_double x && is_non_decreasing x
  ) in
  Printf.printf "Part2: %d\n" count
