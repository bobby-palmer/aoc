open Aoc

let masses =
  Input.get 2019 1
  |> String.split_on_char '\n'
  |> List.map int_of_string

let fuel_required mass = mass / 3 - 2

let () =
  masses
  |> List.map fuel_required
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part1: %d\n"

let rec fuel_required_rec mass =
  let fuel = mass / 3 - 2 in
  if fuel <= 0 then 0
  else fuel + fuel_required_rec fuel

let () =
  masses
  |> List.map fuel_required_rec
  |> List.fold_left ( + ) 0
  |> Printf.printf "Part2: %d\n"
