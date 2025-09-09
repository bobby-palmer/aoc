type direction =
  | North
  | South
  | West
  | East

type location = int * int

type guard = direction * location

let find_guard_opt =
  List.find_mapi (fun row chars -> 
    let guard_opt = List.find_mapi (fun col letter ->
      match letter with
        | '^' -> Some (North, col)
        | '>' -> Some (East, col)
        | '<' -> Some (West, col)
        | 'v' -> Some (South, col)
        | _ -> None
    ) chars in
    match guard_opt with
      | Some (dir, col) -> Some (dir, (row, col))
      | None -> None
  )

let get_blockers input =
  input |> List.mapi (fun row chars ->
    chars |> List.mapi (fun col letter ->
      if letter == '#' then Some (row, col)
      else None
    ) |> List.filter_map (fun elt -> elt)
  ) |> List.flatten

let get_next_pos = function
  | (North, (row, col)) -> (row - 1, col)
  | (South, (row, col)) -> (row + 1, col)
  | (West, (row, col)) -> (row, col - 1)
  | (East, (row, col)) -> (row, col + 1)

let solve_p1 guard blockers =
  0

let solve _input =
  (0, 0)
