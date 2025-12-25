let input = "cqjxjnds"

let increment_char letter =
  let ascii = int_of_char letter in
  char_of_int (ascii + 1)

let increment letters =
  let rec aux = function
    | x :: xs when x = 'z' ->
      'a' :: (aux xs)
    | x :: xs ->
      (increment_char x) :: xs
    | _ -> failwith "Overflowed"
  in
  letters 
  |> List.rev
  |> aux
  |> List.rev

let rec has_straight = function
  | a :: b :: c :: xs ->
    if b = increment_char a && c = increment_char b then true
    else has_straight (b :: c :: xs)
  | _ -> false

let has_forbidden letters =
  List.mem 'i' letters 
  || List.mem 'l' letters
  || List.mem 'o' letters

let rec has_double_double = function
  | a :: b :: xs when a = b ->
    let forbidden = a in
    let rec has_single_double = function
    | a :: b :: _ when a = b && a <> forbidden -> true
    | _ :: xs -> has_single_double xs
    | [] -> false
    in has_single_double xs
  | _ :: xs -> has_double_double xs
  | [] -> false

let get_next_pswd input =
  let rec aux letters =
    if has_straight letters 
      && not (has_forbidden letters) 
      && has_double_double letters then letters
    else aux (increment letters)
  in input
  |> String.to_seq
  |> List.of_seq
  |> increment
  |> aux
  |> List.to_seq
  |> String.of_seq

let () =
  get_next_pswd input |> print_string;
  print_newline ();
  get_next_pswd input |> get_next_pswd |> print_string
