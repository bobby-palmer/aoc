open Aoc

let input = 
  Input.get ~year:2020 ~day:6
  |> String.split_paragraphs

module CharSet = Set.Make(Char)

let count_yesses answers =
  answers
  |> String.to_seq
  |> Seq.filter ((!=) '\n')
  |> CharSet.of_seq
  |> CharSet.cardinal

let part1 input =
  input
  |> List.map count_yesses
  |> List.fold_left (+) 0

let () = part1 input |> Printf.printf "Part1: %d\n"

let count_unanimous group =
  let selections =
    group
    |> String.split_on_char '\n'
    |> List.map (fun line -> line |> String.to_seq |> CharSet.of_seq)
  in
  selections
    |> List.fold_left CharSet.inter (List.hd selections)
    |> CharSet.cardinal

let () =
  input
    |> List.map count_unanimous
    |> List.fold_left (+) 0
    |> Printf.printf "Part2: %d\n"
