open Aoc

type square = Paper | Empty

let square_of_char = function
  | '@' -> Paper
  | '.' -> Empty
  | _ -> failwith "bad char"

let grid =
  Input.get ~year:2025 ~day:4
    |> String.split_on_char '\n'
    |> List.to_seq
    |> Seq.mapi (fun row line ->
        line
          |> String.to_seq
          |> Seq.mapi (fun col letter ->
            ((row, col), square_of_char letter)
          )
    )
    |> Seq.concat
    |> Hashtbl.of_seq

let can_access point =
  let num_blockers =
    Point2d.neighbors point
      |> List.map (Hashtbl.find_opt grid)
      |> List.filter (( = ) (Some Paper))
      |> List.length
  in
  num_blockers < 4

let get_reachable () =
  grid
    |> Hashtbl.to_seq
    |> Seq.filter_map (fun (point, state) ->
        if state = Paper then Some point
        else None
      )
    |> Seq.filter can_access
    |> List.of_seq

let () =
  let accessible = List.length (get_reachable ())
  in Printf.printf "Part1: %d\n" accessible

let reach_recursive () =
  let rec aux acc =
    let reachable = get_reachable () in
    let num_reachable = List.length reachable in
    if num_reachable = 0 then acc
    else (
      List.iter (fun point -> Hashtbl.replace grid point Empty) reachable;
      aux (acc + num_reachable)
    )
  in aux 0

let () =
  let total_reached = reach_recursive () in
  Printf.printf "Part2: %d\n" total_reached
