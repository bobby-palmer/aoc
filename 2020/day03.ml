open Aoc

let input = 
  Input.get ~year:2020 ~day:3
  |> Grid.of_string

let path_of_slope dr dc =
  Seq.unfold (fun (r, c) ->
    let r = r + dr
    and c = c + dc in
    Some ((r, c), (r, c))
  ) (0, 0)

let trees_in_path grid dr dc =
  path_of_slope dr dc
  |> Seq.take_while (fun (r, _) -> r < Grid.rows grid)
  |> Seq.map (fun (r, c) ->
        Grid.get grid r (c mod Grid.cols grid)
      )
  |> Seq.filter ((=) '#')
  |> Seq.length

let part1 grid = trees_in_path grid 1 3

let () = part1 input |> Printf.printf "Part1: %d\n"

let part2 grid =
  let slopes = [
    (1, 1);
    (1, 3);
    (1, 5);
    (1, 7);
    (2, 1);
  ] in
  List.map (fun (dr, dc) -> trees_in_path grid dr dc) slopes
  |> List.fold_left ( * ) 1

let () = part2 input |> Printf.printf "Part2: %d\n"
