open Aoc

let parse_line line = 
  Scanf.sscanf line 
    "%s would %s %d happiness units by sitting next to %[^.]."
    (fun name gain_lost units name2 ->
      let delta =
        if gain_lost = "gain" then units
        else -1 * units
      in
      ((name, name2), delta)
    )

let parse_adj input =
  let changes = input
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map parse_line
  in
  let adj_lst = Hashtbl.create 16 in
  changes
  |> List.map (fun ((f, t), change) ->
    let change2 = List.assoc (t, f) changes in
    ((f, t), change + change2)
  )
  |> List.iter(fun ((f, t), c) -> 
    let current = match Hashtbl.find_opt adj_lst f with
      | Some lst -> lst
      | None -> []
    in
    Hashtbl.replace adj_lst f ((t, c) :: current)
  );
  adj_lst

module StringSet = Set.Make(String)

let rec get_best_cycle adj visited first current =
  let visited = StringSet.add current visited in
  let best_choice =
    Hashtbl.find adj current
    |> List.filter (fun (node, _) -> not (StringSet.mem node visited))
    |> List.map (fun (node, change) ->
      change + get_best_cycle adj visited first node
    )
    |> List.fold_left (fun best_opt opt ->
      match best_opt with
      | Some value -> Some (max value opt)
      | None -> Some opt 
    ) None
  in
  match best_choice with
  | Some value -> value
  | None -> Hashtbl.find adj current |> List.assoc first


let part1 adj =
  let visited = StringSet.empty in
  let first = 
    Hashtbl.to_seq_keys adj 
    |> Seq.find (fun _ -> true)
    |> Option.get
  in
  get_best_cycle adj visited first first

let add_me adj =
  let me = "$BOBBY" in
  let new_tbl =
    Hashtbl.to_seq adj
    |> Seq.map (fun (k, lst) -> (k, (me, 0) :: lst))
    |> Hashtbl.of_seq
  in
  Hashtbl.to_seq_keys adj
  |> Seq.map (fun name -> (name, 0))
  |> List.of_seq
  |> Hashtbl.add new_tbl me;
  new_tbl

let () = 
  let input = Input.get 2015 13 in
  let adj = parse_adj input in
  part1 adj |> print_int;
  print_newline ();
  add_me adj |> part1 |> print_int;
  print_newline ()
