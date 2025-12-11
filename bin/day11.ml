(** Build adjacency list *)
let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line ->
        match String.split_on_char ' ' line with
        | node :: connections -> 
            (String.sub node 0 (String.length node - 1), connections)
        | _ -> failwith "Bad input"
      )
  |> List.to_seq
  |> Hashtbl.of_seq

(** return value of option or default if None *)
let get_or_else default opt =
  match opt with
  | Some v -> v
  | None -> default

(** Return number of distinct paths from s -> e in DAG *)
let calc_num_paths s e graph =
  let cache = Hashtbl.create 0 in
  let rec aux current =
    if current = e then 1
    else match Hashtbl.find_opt cache current with
    | Some dist -> dist
    | None ->
      let dist = Hashtbl.find_opt graph current
      |> get_or_else []
      |> List.map (aux)
      |> List.fold_left (+) 0 in
      Hashtbl.replace cache current dist;
      dist
  in aux s

let part1 input =
  input
  |> parse
  |> calc_num_paths "you" "out"

let part2 input =
  let graph = input |> parse in
  let cnp s e =
    calc_num_paths s e graph in
  cnp "svr" "dac" * cnp "dac" "fft" * cnp "fft" "out" +
  cnp "svr" "fft" * cnp "fft" "dac" * cnp "dac" "out"

let () = 
  let input = Aoc.Input.get_input 11 in
  input |> part1 |> print_int;
  print_newline ();
  input |> part2 |> print_int;
  print_newline();;
