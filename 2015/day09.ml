open Aoc

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line ->
    Scanf.sscanf line "%s to %s = %d" (fun a b dist -> 
      (a, b, dist)
    )
  )

let tbl_append tbl k v =
  let current = 
    match Hashtbl.find_opt tbl k with
    | Some v -> v
    | None -> []
  in Hashtbl.replace tbl k (v :: current)

let mk_adj_lst distance_lst =
  let adj_lst = Hashtbl.create 16 in
  distance_lst
  |> List.iter (fun (a, b, dist) ->
    tbl_append adj_lst a (b, dist);
    tbl_append adj_lst b (a, dist);
  );
  adj_lst

module StringSet = Set.Make(String)

let rec visit_all opt_fold adj_lst visited location =
  let best_opt =
    Hashtbl.find adj_lst location
    |> List.filter (fun (next_location, _) ->
        not @@ StringSet.mem next_location visited
    )
    |> List.map (fun (next_location, distance) ->
      distance + 
        visit_all opt_fold adj_lst (StringSet.add location visited) next_location
    )
    |> List.fold_left opt_fold None
  in
  match best_opt with
  | Some v -> v
  | None -> 0

let solve opt_fold adj_lst =
  let visited = StringSet.empty in
  Hashtbl.to_seq_keys adj_lst
  |> Seq.map (visit_all opt_fold adj_lst visited)
  |> Seq.fold_left opt_fold None
  |> Option.get

let mk_opt_fold f acc value =
  match acc with
  | Some x -> Some (f x value)
  | None -> Some value

let part1 = solve (mk_opt_fold min) 

let part2 = solve (mk_opt_fold max)

let () =
  let dist_lst = parse @@ Input.get 2015 9 in
  let adj_lst = mk_adj_lst dist_lst in
  part1 adj_lst |> print_int;
  print_newline ();
  part2 adj_lst |> print_int;
  print_newline ()
