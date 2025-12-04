type square = Paper | Nothing

let square_of_char = function
  | '@' -> Paper
  | _ -> Nothing

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.to_seq
  |> Seq.mapi (fun row line ->
        line 
        |> String.to_seq 
        |> Seq.mapi (fun col letter -> 
            ((row, col), square_of_char letter)
            )
      )
  |> Seq.flat_map (fun elt -> elt)
  |> Hashtbl.of_seq

let range lower upper =
  Seq.unfold (fun current ->
    if current <= upper then Some (current, current + 1)
    else None
  ) lower

(** Count the number of paper squares in 3 by 3 square centered at r, c*)
let count_in_3by3 grid (r, c) =
  (range (-1) 1) 
  |> Seq.map (fun dr ->
        (range (-1) 1)
        |> Seq.filter (fun dc ->
                Hashtbl.find_opt grid (r + dr, c + dc) = Some Paper
            )
        |> Seq.length
      )
  |> Seq.fold_left (+) 0

let can_reach grid (r, c) =
  (range (-1) 1) 
  |> Seq.map (fun dr ->
        (range (-1) 1)
        |> Seq.filter (fun dc ->
                Hashtbl.find_opt grid (r + dr, c + dc) = Some Paper
            )
        |> Seq.length
      )
  |> Seq.fold_left (+) 0
  |> ((>=) 4)

let part1 input =
  let grid = parse input in
  grid
  |> Hashtbl.to_seq 
  |> Seq.filter_map (fun (pos, square) ->
        if square = Paper then Some pos
        else None
      )
  |> Seq.filter (can_reach grid)
  |> Seq.length

let () = 
  let input = Aoc.Input.get_input 4 in
  input |> part1 |> print_int;
  print_newline ();;
