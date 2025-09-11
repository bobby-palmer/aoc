let parse input =
  input |> String.trim |> String.split_on_char '\n' |> Array.of_list |>
  Array.map (fun str -> String.to_seq str |> Array.of_seq)

type direction =
  | North
  | West
  | East
  | South

let rotate = function
  | North -> East
  | East -> South
  | South -> West
  | West -> North

let move dir row col =
  match dir with
    | North -> (row-1, col)
    | South -> (row+1, col)
    | West -> (row, col-1)
    | East -> (row, col+1)

let direction_of_char = function
  | '^' -> Some North
  | '>' -> Some East
  | '<' -> Some West
  | 'v' -> Some South
  | _ -> None

let find_guard input =
  input |> Array.find_mapi (fun row bytes ->
    bytes |> Array.find_mapi (fun col letter -> 
      match direction_of_char letter with
        | Some dir -> Some (dir, row, col)
        | None -> None
    )
  ) |> Option.get

let arr_get index arr = Array.get arr index

let gen_path grid state =
  let aux state =
    match state with
      | None -> None
      | Some (dir, row, col) ->
          let next = 
            try
              let (nrow, ncol) = move dir row col in
              if grid.(nrow).(ncol) = '#' then Some (rotate dir, row, col)
              else Some (dir, nrow, ncol)
            with _ -> None
          in
          Some ((dir, row, col), next)
  in Seq.unfold aux (Some state)

let map_locations seq =
  let tbl = Hashtbl.create 0 in
  seq |> Seq.filter_map (fun (_, row, col) ->
    if Hashtbl.find_opt tbl (row, col) |> Option.is_some then None
    else (
      Hashtbl.replace tbl (row, col) ();
      Some (row, col)
    )
  )

let part1 input =
  let input = parse input in
  let guard = find_guard input in
  gen_path input guard |> map_locations |> Seq.length

let is_inf seq =
  let tbl = Hashtbl.create 0 in
  seq |> Seq.exists (fun state ->
    if Hashtbl.find_opt tbl state |> Option.is_some then true
    else (
      Hashtbl.replace tbl state ();
      false
    )
  )

let part2 input =
  let input = parse input in
  let guard = find_guard input in
  gen_path input guard |> map_locations |> List.of_seq |>
  List.fold_left (fun acc (row, col) ->
    if input.(row).(col) = '.' then (
      input.(row).(col) <- '#';
      let inf = gen_path input guard |> is_inf in
      input.(row).(col) <- '.';

      if inf then acc + 1
      else acc
    )
    else acc
  ) 0


let solve input =
  let p1 = part1 input in
  let p2 = part2 input in
  (p1, p2)
