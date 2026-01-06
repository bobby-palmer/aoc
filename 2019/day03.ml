open Aoc

let input = Input.get 2019 3

let parse_dir s =
  Scanf.sscanf s "%c%d" (fun dir count ->
    let dir =
      match dir with
      | 'U' -> Vec2.north
      | 'D' -> Vec2.south
      | 'L' -> Vec2.west
      | 'R' -> Vec2.east
      | _ -> failwith "Bad direction"
    in (dir, count)
  )

let parse_line s =
  s 
  |> String.split_on_char ','
  |> List.map parse_dir

let (_path_a, _path_b) =
  match String.split_on_char '\n' input with
  | a :: b :: [] -> (parse_line a, parse_line b)
  | _ -> failwith "Bad input"
