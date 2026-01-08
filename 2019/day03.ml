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

let (path_a, path_b) =
  match String.split_on_char '\n' input with
  | a :: b :: [] -> (parse_line a, parse_line b)
  | _ -> failwith "Bad input"

let trace_path path =
  path
    |> List.to_seq
    |> Seq.flat_map (fun (direction, count) ->
        Seq.repeat direction |> Seq.take count
    )
    |> Seq.scan Vec2.add Vec2.zero
    (* Drop starting position *)
    |> Seq.drop 1

module PointSet = Set.Make(Vec2)

let () =
  let positions_a = trace_path path_a |> PointSet.of_seq in
  let closest =
    trace_path path_b
      |> Seq.filter (fun pos -> PointSet.mem pos positions_a)
      |> Seq.map (Vec2.manhattan_distance Vec2.zero)
      |> Seq.fold_left min Int.max_int
  in
  Printf.printf "Part1: %d\n" closest

let make_tbl positions =
  let tbl = Hashtbl.create 16 in
  Seq.iteri (fun i pos ->
    let current =
      match Hashtbl.find_opt tbl pos with
      | Some other_i -> min other_i i
      | None -> i
    in
    Hashtbl.replace tbl pos current
  ) positions;
  tbl

let () =
  let positions_a =
    path_a
      |> trace_path
      |> make_tbl
  in
  let closest =
    path_b
      |> trace_path
      |> Seq.mapi (fun i pos -> (pos, i))
      |> Seq.filter_map (fun (pos, i) ->
          Hashtbl.find_opt positions_a pos
            |> Option.map (( + ) i)
      )
      |> Seq.fold_left min Int.max_int
  (* Add 2 because we skip the origin in the path sequence *)
  in Printf.printf "Part2: %d\n" (closest + 2)
