let parse input =
  input |> String.trim |> String.split_on_char '\n' |> Array.of_list |>
  Array.map (fun str -> str |> String.to_seq |> Array.of_seq)

let find_antena grid =
  let tbl = Hashtbl.create 0 in
  grid |> Array.iteri (fun row letters ->
    letters |> Array.iteri (fun col letter ->
      if letter != '.' then
        let current = match Hashtbl.find_opt tbl letter with
          | None -> []
          | Some lst -> lst
        in
        Hashtbl.replace tbl letter ((row, col) :: current)
    )
  );
  tbl

let project a b =
  let (r1, c1) = a and (r2, c2) = b in
  (r2 + (r2 - r1), c2 + (c2 - c1))

let is_in_grid rows cols point =
  let (row, col) = point in
  row >= 0 && row < rows && col >= 0 && col < cols

let get_places lst rows cols =
  List.to_seq lst |> Seq.flat_map (fun a ->
    List.to_seq lst |> Seq.filter_map (fun b -> 
      if a = b then None
      else Some (project a b)
    )
  ) |> Seq.filter (is_in_grid rows cols)

let part1 input =
  let grid = parse input in
  let rows = Array.length grid
  and cols = Array.length (grid.(0)) in
  let tbl = find_antena grid in
  let places = Hashtbl.create 0 in
  Hashtbl.to_seq_values tbl |> 
    Seq.iter (fun lst ->
      get_places lst rows cols |> Seq.iter (fun place ->
        Hashtbl.replace places place ()
      )
    );
  Hashtbl.length places

let solve input =
  let p1 = part1 input in
  (p1, p1)
