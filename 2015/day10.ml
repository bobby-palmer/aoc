let input = "1321131112"

let chars_of_int num =
  num
  |> string_of_int
  |> String.to_seq
  |> List.of_seq

let look_and_say digit_lst =
  let rec aux length acc = function
    | a :: b :: xs when a = b ->
      aux (length + 1) acc (b :: xs)
    | a :: xs ->
      aux 0 (a :: (chars_of_int (length + 1) |> List.rev) @ acc) xs
    | [] -> List.rev acc
  in
  aux 0 [] digit_lst

let range n =
  Seq.unfold (fun current ->
    if current < n then Some (current, current + 1)
    else None
  ) 0

let solve n input =
  let digit_lst =
    input 
    |> String.to_seq 
    |> List.of_seq
  in
  range n
  |> Seq.fold_left (fun digit_lst _ ->
    look_and_say digit_lst
  ) digit_lst
  |> List.length

let () =
  solve 40 input |> print_int;
  print_newline ();
  solve 50 input |> print_int;
  print_newline ()
