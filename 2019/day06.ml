open Aoc

let input = Input.get 2019 6

let parse_line line =
  match String.split_on_char ')' line with
  (* Reverse order to (orbiter, orbitee) *)
  | a :: b :: [] -> (b, a)
  | _ -> failwith "Bad line"

let orbits =
  input
  |> String.split_on_char '\n'
  |> List.map parse_line
  |> List.to_seq
  |> Hashtbl.of_seq

let count_orbitees self orbiter =
  match Hashtbl.find_opt orbits orbiter with
  | Some orbitee -> 1 + self orbitee
  | None -> 0

let memoize f = 
  let tbl = Hashtbl.create 16 in
  let rec g x =
    try
      Hashtbl.find tbl x
    with
    | Not_found ->
        let y = f g x in
        Hashtbl.add tbl x y;
        y
  in 
  g

let count_orbitees = memoize count_orbitees

let () =
  let orbit_count =
    orbits
      |> Hashtbl.to_seq_keys
      |> Seq.map count_orbitees
      |> Seq.fold_left ( + ) 0
  in Printf.printf "Part1: %d\n" orbit_count
