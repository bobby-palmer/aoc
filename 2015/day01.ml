open Aoc

let apply_letter floor = function
  | '(' -> floor + 1
  | ')' -> floor - 1
  | _ -> failwith "Bad input"

let part1 input =
  input
  |> String.to_seq
  |> Seq.fold_left apply_letter 0

type data = Result of int | Floor of int

let part2 input =
  let maybe_result = input
  |> String.to_seq
  |> Seq.fold_lefti (fun d idx letter ->
    match d with
      | Floor floor ->
        let next_floor = apply_letter floor letter in
        if next_floor < 0 then Result idx
        else Floor next_floor
      | _ -> d
  ) (Floor 0) in
  match maybe_result with
    (* Add 1 to account for zero based indexing *)
    | Result result -> result + 1
    | _ -> failwith "Input never went negative"

let () = 
  let input = Input.get 2015 1 in
  part1 input |> print_int;
  print_newline ();
  part2 input |> print_int;
  print_newline ()

