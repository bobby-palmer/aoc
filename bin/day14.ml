open Aoc

let input = Input.get_input 14

let grid_size = (101, 103)

let parse input =
  input |> String.trim |> String.split_on_char '\n'
    |> List.map (fun str -> Scanf.sscanf str "p=%d,%d v=%d,%d" (fun a b c d -> (a, b, c, d)))

let (%!) a b =
  let m = a mod b in
  if m < 0 then m + b
  else m

let move time (x, y, dx, dy) =
  let (mx, my) = grid_size in
  ((x + dx * time) %! mx, (y + dy * time) %! my)

let quadrant_of_pos (x, y) =
  let (mx, my) = grid_size in
  if x < mx / 2 && y < my / 2 then Some "one"
  else if x < mx / 2 && y > my / 2 then Some "two"
  else if x > mx / 2 && y < my / 2 then Some "three"
  else if x > mx / 2 && y > my / 2 then Some "four"
  else None

let count_of_opt_list lst =
  let tbl = Hashtbl.create 0 in
  lst |> List.iter (fun elt ->
    match elt with
      | Some quad ->
          let current = match Hashtbl.find_opt tbl quad with
            | Some num -> num
            | None -> 0
          in
          Hashtbl.replace tbl quad (current + 1)
      | None -> ()
  );
  Hashtbl.to_seq_values tbl

let mult a b = a * b

let part1 input =
  input |> parse |> List.map (move 100) |> List.map (quadrant_of_pos) |> count_of_opt_list |> Seq.fold_left (mult) 1

let () =
  Printf.printf "%d" (part1 input)
    
