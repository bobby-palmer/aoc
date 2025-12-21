let parse input =
  input
  |> String.trim
  |> String.split_on_char ','
  |> List.map (fun line ->
        Scanf.sscanf line "%d-%d" (fun a b -> (a, b))
      )

(** Raise base^pow*)
let rec pow base exp =
  if exp < 0 then failwith "Bad exponent"
  else if exp = 0 then 1
  else base * pow base (exp - 1)

(** Seq over [low, high]*)
let range (low, high) =
  Seq.unfold (fun cur ->
      if cur <= high then Some (cur, cur + 1)
      else None
    ) low

(** Return true if number can be made of len k identical numbers*)
let repeats num k =
  let digits = num |> string_of_int |> String.length in
  if k <= 0 then false
  else if digits mod k <> 0 then false
  else
    let split = pow 10 k in
    let target = num mod split in
    let rec aux rem =
      if rem = 0 then true
      else if (rem mod split) <> target then false
      else aux (rem / split)
    in aux num

(** Return the sum of all invalid ids in input *)
let solve is_invalid input =
  input
  |> parse
  |> List.to_seq
  |> Seq.flat_map range
  |> Seq.filter is_invalid
  |> Seq.fold_left (+) 0

let part1 = solve (fun id ->
    let digits = id |> string_of_int |> String.length in
    if digits mod 2 = 0 then repeats id (digits / 2)
    else false
  )

let part2 = solve (fun id ->
    let digits = id |> string_of_int |> String.length in
    range (1, digits / 2)
    |> Seq.filter (repeats id)
    |> Seq.is_empty
    |> not
  )

let () = 
  let input = Aoc.Input.get_input 2 in
  part1 input |> print_int;
  print_newline ();
  part2 input |> print_int;
  print_newline ()
