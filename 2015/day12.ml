open Aoc

type value = 
  | Number of int 
  | String of string
  | Array of value List.t
  | Object of (string * value) List.t

let is_digit = function
  | '0' .. '9' -> true
  | _ -> false

let ( <|> ) a b =
  match a with
  | Ok _ -> a
  | Error _ -> b ()

let rec parse_number letters = 
  let rec aux acc = function
    | x :: xs when is_digit x ->
      aux (10 * acc + (int_of_char x - int_of_char '0')) xs
    | remainder -> (acc, remainder)
  in
  match letters with
    | '-' :: xs ->
      parse_number xs 
      |> Result.map (fun (number, xs) -> 
            (-1 * number, xs)
      )
    | x :: _ when is_digit x -> Ok (aux 0 letters)
    | _ -> Error ()

and parse_string letters =
  let rec aux acc = function
    | '\"' :: xs -> (acc |> List.rev |> List.to_seq |> String.of_seq, xs)
    | x :: xs -> aux (x :: acc) xs
    | _ -> failwith "Unclosed string"
  in
  match letters with
    | '\"' :: xs ->
      let (s, rest) = aux [] xs in
      Ok (s, rest)
    | _ -> Error ()

and parse_array letters =
  let rec aux acc = function
    | ']' :: xs -> (List.rev acc, xs)
    | ',' :: xs | '[' :: xs ->
      (match parse_value xs with
        | Ok (value, rest) -> aux (value :: acc) rest
        | _ -> failwith "Bad array value")
    | _ -> failwith "Bad array delimiter"
  in
  match letters with
    | '[' :: _ -> Ok (aux [] letters)
    | _ -> Error ()

and parse_member letters =
  let (s, xs) = parse_string letters |> Result.get_ok in
  match xs with
    | ':' :: xs ->
      let (value, xs) = parse_value xs |> Result.get_ok in
      (s, value, xs)
    | _ -> failwith "Bad member"

and parse_object letters =
  let rec aux acc = function
    | '}' :: xs -> (List.rev acc, xs)
    | '{' :: xs | ',' :: xs ->
      let (s, value, xs) = parse_member xs in
      aux ((s, value) :: acc) xs
    | _ -> failwith "Bad object delimiter"
  in
  match letters with
    | '{' :: _ -> Ok (aux [] letters)
    | _ -> Error ()

and parse_value letters = 
  (parse_string letters |> Result.map (fun (s, xs) -> (String s, xs)))
  <|> fun () -> (parse_number letters |> Result.map (fun (num, xs) -> (Number num, xs)))
  <|> fun () -> (parse_array letters |> Result.map (fun (values, xs) -> (Array values, xs)))
  <|> fun () -> (parse_object letters |> Result.map (fun (members, xs) -> (Object members, xs)))

let rec sum_of_json = function
  | Number num -> num
  | String _ -> 0
  | Array values ->
      values 
      |> List.map sum_of_json 
      |> List.fold_left (+) 0
  | Object members ->
      members
      |> List.map (fun (_, value) -> sum_of_json value)
      |> List.fold_left (+) 0

let rec filter_json = function
  | Object members ->
    if List.exists (fun (_, value) -> value = String "red") members then Object []
    else Object (members |> List.map (fun (s, value) -> (s, filter_json value)))
  | Array values -> Array (values |> List.map filter_json)
  | other -> other 

let () =
  let input = Input.get 2015 12 in
  let letters = input 
    |> String.to_seq 
    |> Seq.filter (fun l -> l <> ' ' && l <> '\n' && l <> '\r')
    |> List.of_seq
  in
  let (json, _) = parse_value letters |> Result.get_ok in
  sum_of_json json |> print_int;
  print_newline ();
  filter_json json |> sum_of_json |> print_int;
  print_newline ()
