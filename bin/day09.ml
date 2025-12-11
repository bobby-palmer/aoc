let parse input = 
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line ->
        Scanf.sscanf line "%d,%d" (fun a b -> (a, b))
      )

let area (r1, c1) (r2, c2) =
  (1 + abs (r1 - r2)) * (1 + abs(c1 - c2))

let part1 input =
  let lst = input |> parse in
  lst
  |> List.map (fun corner1 ->
        lst
        |> List.map (fun corner2 ->
              area corner1 corner2
            )
        |> List.fold_left max 0
      )
  |> List.fold_left max 0

let () =
  let input = Aoc.Input.get_input 9 in
  input |> part1 |> print_int;
