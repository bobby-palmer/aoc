let rec read_pairs acc1 acc2 =
  try
    let (a, b) = Scanf.scanf "%d %d\n" (fun a b -> (a, b)) in
    read_pairs (a :: acc1) (b :: acc2)
  with End_of_file ->
    (List.rev acc1, List.rev acc2)

let rec solve_part_1 la lb =
  match la, lb with
    | a::rest_a, b::rest_b ->
        abs (a - b) + solve_part_1 rest_a rest_b
    | [], [] -> 0
    | _ -> failwith "Lists differ in length"

let solve_part_2 la lb =
  let table = Hashtbl.create (List.length lb) in
  List.iter (fun x -> 
    let count = try Hashtbl.find table x with Not_found -> 0 in
    Hashtbl.replace table x (count + 1)
  ) lb;
  List.fold_left (fun acc x ->
    let count = try Hashtbl.find table x with Not_found -> 0 in
    acc + x * count
  ) 0 la

let () = 
  let (l1, l2) = read_pairs [] [] in
  let s1 = List.sort compare l1 
  and s2 = List.sort compare l2 in
  let p1 = solve_part_1 s1 s2 
  and p2 = solve_part_2  s1 s2 in
  Printf.printf "P1: %d, P2: %d" p1 p2

