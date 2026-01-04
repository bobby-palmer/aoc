open Aoc

let lights = 
  Input.get 2015 18
  |> Grid.of_string
  |> Grid.map (( = ) '#')
