let input = Aoc.Input.get_input 11

let parse input =
  input |> String.trim |> String.split_on_char ' ' |> List.map (int_of_string)

let rec num_digits num =
  if num = 0 then 0
  else 1 + num_digits (num / 10)

let rec pow_ten pow =
  if pow = 0 then 1
  else 10 * pow_ten (pow - 1)

let split_digits num =
  let digits = num_digits num in
  let split = pow_ten (digits / 2) in
  (num / split, num mod split)
  
let hashtbl_plus_eq tbl k v =
  let current = match Hashtbl.find_opt tbl k with
    | Some v -> v
    | None -> 0
  in
  Hashtbl.replace tbl k (current + v)

let do_blink tbl =
  let new_tbl = Hashtbl.create 0 in
  let hashtbl_plus_eq = hashtbl_plus_eq new_tbl in
  tbl |> Hashtbl.to_seq |> Seq.iter (fun (num, cnt) ->
    if num = 0 then 
      hashtbl_plus_eq 1 cnt
    else if num_digits num mod 2 = 0 then
      let (l, r) = split_digits num in
      hashtbl_plus_eq l cnt;
      hashtbl_plus_eq r cnt
    else
      hashtbl_plus_eq (num * 2024) cnt
    ;
  );
  new_tbl

let rec repeat_blink cnt tbl =
  if cnt = 0 then tbl
  else repeat_blink (cnt - 1) (do_blink tbl)

let counts_of_list lst = 
  let tbl = Hashtbl.create 0 in
  lst |> List.iter (fun elt -> hashtbl_plus_eq tbl elt 1);
  tbl

let part1 input =
  input |> parse |> counts_of_list |> repeat_blink 25 |> Hashtbl.to_seq_values |> Seq.fold_left (+) 0

let part2 input =
  input |> parse |> counts_of_list |> repeat_blink 75 |> Hashtbl.to_seq_values |> Seq.fold_left (+) 0

let () =
  Printf.printf "%d, %d" (part1 input) (part2 input)
