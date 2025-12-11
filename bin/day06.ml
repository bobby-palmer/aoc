let words_of_string s =
  s
  |> String.split_on_char ' '
  |> List.filter (fun elt -> String.length elt > 0)

let parse input =
  let lines = input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.rev in
  match lines with
  | ops :: nums ->
    let ops = ops
      |> words_of_string 
      |> List.to_seq and
    nums = nums
      |> List.to_seq
      |> Seq.map(fun num_line ->
            num_line
            |> words_of_string
            |> List.to_seq
            |> Seq.map(int_of_string)
          ) 
      |> Seq.transpose in
    Seq.zip ops nums
  | _ -> failwith "Bad input"

let part1 input =
  input
  |> parse
  |> Seq.map (fun (op, nums) ->
        match op with
        | "+" -> Seq.fold_left ( + ) 0 nums
        | "*" -> Seq.fold_left ( * ) 1 nums
        | _ -> failwith "Bad op"
      )
  |> Seq.fold_left (+) 0

(** TODO implement rest of parsing *)
(* let part2 input = *)
(*   input *)
(*   |> String.trim *)
(*   |> String.split_on_char '\n' *)
(*   |> List.to_seq *)
(*   |> Seq.map (fun line -> *)
(*         line *)
(*         |> String.to_seq *)
(*         |> List.of_seq *)
(*         |> List.rev *)
(*         |> List.to_seq *)
(*       ) *)
(*   |> Seq.transpose *)

let () =
  let input = Aoc.Input.get_input 6 in
  input |> part1 |> print_int;
  print_newline ();;
