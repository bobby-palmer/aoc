open Aoc

type spec_t = Key | Lock

type spec = {
  kind: spec_t;
  heights: int array;
}

let spec_of_seq seq =
  None

let parse input =
  input
    |> String.trim
    |> String.split_on_char '\n'
    |> List.to_seq

let input = Input.get_input 25
