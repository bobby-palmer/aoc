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

let index_of_tup (a, b, c, d) =
  (a + 9) +
  (b + 9) * 20 +
  (c + 9) * 20 * 20 +
  (d + 9) * 20 * 20 * 20

let window_5_of_seq seq =
  let aux ((prev, a, b, c), seq) =
    match seq () with
      | Seq.Cons (x, xs) ->
          Some ((prev, a, b, c, x), ((a, b, c, x), xs))
      | Seq.Nil -> None
  in
  match seq |> Seq.take 4 |> List.of_seq with
    | a::b::c::d::[] -> Seq.unfold (aux) ((a,b,c,d), Seq.drop 4 seq)
    | _ -> failwith "Not enough elements"

let nanners_of_seq s =
  let tbl = Hashtbl.create 0 in
  s 
    |> window_5_of_seq
    |> Seq.filter_map (
      fun (prev, a, b, c, d) ->
        let idx = index_of_tup (a - prev, b - a, c - b, d - c) in
        if Hashtbl.mem tbl idx then None
        else (
          Hashtbl.add tbl idx ();
          Some (idx, d)
        )
    )


let part2 input =
  let tbl = Hashtbl.create 0 in
  let add (k, v) =
    let current = match Hashtbl.find_opt tbl k with
      | Some v -> v
      | None -> 0
    in Hashtbl.replace tbl k (current + v)
  in
  input 
    |> parse 
    |> List.map (prices_seq)
    |> List.iter (
      fun elt ->
        elt 
          |> Seq.map (fun num -> num mod 10) 
          |> nanners_of_seq 
          |> Seq.iter (add)
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
