open Aoc

let input = Input.get_input 15

let parse input =
  let rec aux before = function
    | [] -> (List.rev before, [])
    | "" :: rest -> (List.rev before, rest)
    | x :: xs -> aux (x :: before) xs
  in
  input |> String.trim |> String.split_on_char '\n' |> aux []

type cell = Open | Wall | Box

let build_grid lst =
  let tbl = Hashtbl.create 0 in
  lst |> List.iteri (fun row letters ->
    letters |> String.iteri (fun col letter ->
      let c = match letter with
        | '#' -> Wall
        | 'O' -> Box
        | '.' | '@' -> Open
        | _ -> failwith "Bad letter"
      in
      Hashtbl.replace tbl (row, col) c
    )  
  );
  tbl

let find_guard lst =
  lst |> List.find_mapi (fun row letters ->
    match String.index_from_opt letters 0 '@' with
      | Some col -> Some (row, col)
      | None -> None
  ) |> Option.get

type direction = Up | Down | Left | Right

let parse_directions lst = 
  lst |> List.map (fun str ->
    str |> String.to_seq |> Seq.map (fun letter ->
      match letter with
        | '^' -> Up
        | 'v' -> Down
        | '<' -> Left
        | '>' -> Right
        | _ -> failwith "Bad letter"
    ) |> List.of_seq
  ) |> List.concat

let do_move grid dir (row, col) = 
  0
