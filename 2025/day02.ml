open Aoc

let parse_range s =
  Scanf.sscanf s "%d-%d" (fun a b -> (a, b))

let ranges =
  Input.get ~year:2025 ~day:2 |>
  String.split_on_char ',' |>
  List.map parse_range

let rec count_digits num =
  if num = 0 then 0
  else 1 + count_digits (num / 10)

let rec pow10 exp =
  if exp = 0 then 1
  else 10 * pow10 (exp - 1)

let repeats times id =
  let num_digits = count_digits id in
  if num_digits mod times != 0 then 
    false
  else
    let base = pow10 (num_digits / times) in
    let rec aux times id =
      if times < 1 then
        failwith "bad number of times"
      else if times = 1 then 
        true
      else
        let remainder = id / base in
        if id mod base = remainder mod base then
          aux (times - 1) remainder
        else
          false
    in aux times id

let is_invalid id =
  repeats 2 id

let sum_repeats ranges is_invalid =
  ranges |>
  List.to_seq |>
  Seq.flat_map (fun (left, right) ->
    Seq.unfold (fun current -> 
      if current <= right then
        Some (current, current + 1)
      else
        None
    ) left
  ) |>
  Seq.filter is_invalid |>
  Seq.fold_left ( + ) 0

let () =
  let total_invalid = sum_repeats ranges is_invalid in
  Printf.printf "Part1: %d\n" total_invalid

let is_invalid id =
  let num_digits = count_digits id in
  Seq.unfold (fun current ->
    if current <= num_digits then
      Some (current, current + 1)
    else
      None
  ) 2 |>
  Seq.find (fun times -> repeats times id) |>
  Option.is_some

(** TODO maybe this could be nicer *)
let () =
  let total_invalid = sum_repeats ranges is_invalid in
  Printf.printf "Part1: %d\n" total_invalid
