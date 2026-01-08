open Aoc

let input = Input.get 2019 6

let parse_line line =
  match String.split_on_char ')' line with
  | a :: b :: [] -> b, a
  | _ -> failwith "Bad line"

(* Construct node to parent mapping *)
let orbits =
  input
  |> String.split_on_char '\n'
  |> List.to_seq
  |> Seq.map parse_line
  |> Hashtbl.of_seq

let memoize f =
  let cache = Hashtbl.create 16 in
  let rec g x =
    if Hashtbl.mem cache x then
      Hashtbl.find cache x
    else
      let y = f g x in
      Hashtbl.add cache x y;
      y
  in 
  g

let count_orbiting self node =
  match Hashtbl.find_opt orbits node with
  | Some parent -> 1 + self parent
  | None -> 0

let () =
  let count_orbiting = memoize count_orbiting in
  let total_orbits =
    Hashtbl.to_seq_keys orbits
      |> Seq.map count_orbiting
      |> Seq.fold_left ( + ) 0
  in
  Printf.printf "Part1: %d\n" total_orbits

let trace_up node =
  Seq.iterate (fun node ->
    Option.bind node (Hashtbl.find_opt orbits)
  ) (Some node)
  |> Seq.take_while Option.is_some
  |> Seq.mapi (fun idx node_opt -> 
      (Option.get node_opt, idx)
  )

let () =
  let a = Hashtbl.find orbits "SAN" in
  let b = Hashtbl.find orbits "YOU" in
  let distances_from_a =
    trace_up a |> Hashtbl.of_seq
  in
  let distance =
    trace_up b
    |> Seq.filter_map (fun (node, distance) ->
        Hashtbl.find_opt distances_from_a node
        |> Option.map (fun d -> distance + d)
    )
    |> Seq.fold_left min Int.max_int
  in
  Printf.printf "Part2: %d\n" distance
