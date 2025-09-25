let parse input =
  input
    |> String.trim
    |> String.split_on_char '\n'
    |> List.map (int_of_string)

let generate_next_secret num =
  let num = num lxor (num * 64) mod 16777216 in
  let num = num lxor (num / 32) mod 16777216 in
  num lxor (num * 2048) mod 16777216

let prices_seq init =
  Seq.unfold (fun num -> Some (num, generate_next_secret num)) init
    |> Seq.take 2001

let part1 input =
  input
    |> parse
    |> List.map (
      fun init ->
        init
          |> prices_seq
          |> Seq.fold_left (fun _ elt -> elt) 0
    )
    |> List.fold_left (+) 0

let nanners_of_seq s =
  let tbl = Hashtbl.create 0 in
  let tbl_remember k v =
    match Hashtbl.find_opt tbl k with
      | Some _ -> ()
      | None -> Hashtbl.replace tbl k v
  in
  let rec aux = function
    | prev::a::b::c::d::xs ->
        tbl_remember (a - prev, b - a, c - b, d - c) d;
        aux (a::b::c::d::xs)
    | _ -> ()
  in aux (List.of_seq s);
  tbl

let part2 input =
  let tbl = Hashtbl.create 0 in
  let merge other_tbl =
    other_tbl 
      |> Hashtbl.to_seq
      |> Seq.iter (
        fun (k, v) ->
          let current = match Hashtbl.find_opt tbl k with
            | Some v -> v
            | None -> 0
          in Hashtbl.replace tbl k (current + v)
      )
  in
  input 
    |> parse 
    |> List.map (prices_seq)
    |> List.iter (
      fun elt ->
        elt 
          |> Seq.map (fun num -> num mod 10) 
          |> nanners_of_seq 
          |> merge
    );
  tbl
    |> Hashtbl.to_seq_values
    |> Seq.fold_left (max) 0

let input = Aoc.Input.get_input 22

let () =
  print_int (part1 input);
  print_newline ();
  print_int (part2 input);
  print_newline ();
