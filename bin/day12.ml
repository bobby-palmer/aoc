open Aoc

let input = Input.get_input 12

let parse input = 
  input |> String.trim |> String.split_on_char '\n'

let mk_tbl input =
  let tbl = Hashtbl.create 0 in
  input |> List.iteri (fun row str ->
    str |> String.iteri (fun col letter ->
      Hashtbl.add tbl (row, col) letter
    )
  );
  tbl

type direction =
  | North
  | South
  | East
  | West

let move_in_direction point dir =
  let (dr, dc) = match dir with
    | North -> (-1, 0)
    | South -> (1, 0)
    | East -> (0, 1)
    | West -> (0, -1)
  and (row, col) = point in
  (row + dr, col + dc)

let directions = [
  North;
  South;
  East;
  West;
]

let rec extract_group tbl coord =
  match Hashtbl.find_opt tbl coord with
    | Some letter ->
        Hashtbl.remove tbl coord;
        let neighbors = directions |> List.map (fun dir ->
          let neighbor = move_in_direction coord dir in
          if Hashtbl.find_opt tbl neighbor = Some letter then 
            extract_group tbl neighbor
          else []

        ) |> List.concat in
        coord :: neighbors

    | None -> []

let extract_groups tbl =
  tbl |> Hashtbl.to_seq_keys |> Seq.map (extract_group tbl)

let get_perimeter lst =
  lst |> List.map (fun coord ->
    directions |> List.map (fun delta ->
      if List.mem (move_in_direction coord delta) lst then 0
      else 1
    ) |> List.fold_left (+) 0
  ) |> List.fold_left (+) 0

let part1 input =
  input |> parse |> mk_tbl |> extract_groups |> Seq.map (fun lst -> List.length lst * get_perimeter lst) |> Seq.fold_left (+) 0

let get_sides lst =
  let contrib_side coord dir =
    let lst_contains coord = lst |> List.mem coord in
    lst_contains coord && not (lst_contains (move_in_direction coord dir))
  in
  lst |> List.map (fun coord ->
    directions |> List.map (fun dir ->
      let super_ord = match dir with
        | North | South -> West
        | West | East -> North
      in
      if contrib_side coord dir && not (contrib_side (move_in_direction coord super_ord) dir)
        then 1
      else 0
    ) |> List.fold_left (+) 0
  ) |> List.fold_left (+) 0

let part2 input =
  input |> parse |> mk_tbl |> extract_groups |> Seq.map (fun lst -> List.length lst * get_sides lst) |> Seq.fold_left (+) 0

let () = 
  Printf.printf "%d, %d" (part1 input) (part2 input)
