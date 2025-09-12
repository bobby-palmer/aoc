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

module PairOrd = struct
  type t = int * int
  let compare = compare
end

module PairSet = Set.Make(PairOrd)

let count_distint_pairs seq =
  PairSet.empty |> PairSet.add_seq seq |> PairSet.cardinal

let sub a b =
  let (r1, c1) = a and (r2, c2) = b in
  (r1 - r2, c1 - c2)

let line_of_points rows cols a b =
  let (dr, dc) = sub a b in
  let aux op state =
    let (row, col) = state in
    if is_in_grid rows cols (row, col) then
      Some ((row, col), (op row dr, op col dc))
    else None
  in
  let forward = Seq.unfold (aux (+)) a and
  backwards = Seq.unfold (aux (-)) a in
  Seq.append forward backwards

let gen_pairs lst =
  let rec aux = function
    | [] -> Seq.empty
    | x :: xs ->
        let seq1 = xs |> List.to_seq |> Seq.map (fun y -> (x, y)) in
        Seq.append seq1 (aux xs)
  in
  aux lst

let part1 input =
  let grid = parse input in
  let rows = Array.length grid
  and cols = Array.length (grid.(0)) in
  let tbl = find_antena grid in
  Hashtbl.to_seq_values tbl |> Seq.flat_map (fun lst -> 
    get_places lst rows cols
  ) |> count_distint_pairs

let part2 input =
  let grid = parse input in
  let rows = Array.length grid
  and cols = Array.length (grid.(0)) in
  let tbl = find_antena grid in
  Hashtbl.to_seq_values tbl |> Seq.flat_map (fun lst ->
    lst |> gen_pairs |> Seq.flat_map (fun (a, b) -> line_of_points rows cols a b)
  ) |> count_distint_pairs 

let solve input =
  let p1 = part1 input in
  let p2 = part2 input in
  (p1, p2)
