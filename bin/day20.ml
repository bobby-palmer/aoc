type pos_type = Track | Wall | Start | End

let pos_type_of_char = function
  | 'S' -> Start
  | 'E' -> End
  | '.' -> Track
  | '#' -> Wall
  | _ -> failwith "Invalid letter"

let pos_type_is_open = function
  | Start | End | Track -> true
  | _ -> false

let parse input =
  input 
  |> String.trim 
  |> String.split_on_char '\n'
  |> List.to_seq
  |> Seq.mapi (
    fun row letters ->
      letters 
        |> String.to_seq
        |> Seq.mapi (
          fun col letter ->
            ((row, col), pos_type_of_char letter)
        )
  )
  |> Seq.concat
  |> Hashtbl.of_seq

let neighbors (row, col) = 
  [
    (1, 0);
    (-1, 0);
    (0, 1);
    (0, -1);
  ]
  |> List.map (
    fun (dr, dc) -> (row + dr, col + dc)
  )

let bfs graph start_pos =
  let tbl = Hashtbl.create 0 in
  let rec aux depth to_visit: unit =
    if List.is_empty to_visit then ()
    else 
      to_visit |> List.iter (fun pos -> Hashtbl.replace tbl pos depth);
      let next = to_visit
        |> List.map (neighbors)
        |> List.flatten
        |> List.filter (
          fun pos ->
            not (Hashtbl.mem tbl pos) && 
            (match Hashtbl.find_opt graph pos with
                | Some(Start) | Some(End) | Some(Track) -> true
                | _ -> false)
        )
      in aux (depth + 1) next
  in
  aux 0 [start_pos];
  tbl
