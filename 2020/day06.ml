open Aoc

let input = 
  Input.get ~year:2020 ~day:6
  |> String.split_paragraphs

let count_yesses answers =
  answers
  |> String.to_seq
  |> Seq.filter ((!=) '\n')
  |> Seq.map (fun e -> (e, ()))
  |> Hashtbl.of_seq
  |> Hashtbl.length

let part1 input =
  input
  |> List.map count_yesses
  |> List.fold_left (+) 0

(** TODO submit *)
let () = part1 input |> Printf.printf "Part1: %d\n"
