open Aoc

type source =
  | NonaryOp of string
  | UnaryOp of (int -> int) * string
  | BinaryOp of (int -> int -> int) * string * string

let parse_line line =
  match String.split_on_char ' ' line with
  | value :: "->" :: dest :: [] -> 
    (dest, NonaryOp value)
  | "NOT" :: a :: "->" :: dest :: [] ->
    (dest, UnaryOp ((lnot), a))
  | a :: "LSHIFT" :: b :: "->" :: dest :: [] ->
    (dest, UnaryOp ((fun x -> x lsl (int_of_string b)), a))
  | a :: "RSHIFT" :: b :: "->" :: dest :: [] ->
    (dest, UnaryOp ((fun x -> x lsr (int_of_string b)), a))
  | a :: "AND" :: b :: "->" :: dest :: [] ->
    (dest, BinaryOp ((land), a, b))
  | a :: "OR" :: b :: "->" :: dest :: [] ->
    (dest, BinaryOp ((lor), a, b))
  | _ -> failwith "Bad input"

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse_line
  |> List.to_seq
  |> Hashtbl.of_seq

let rec get_source_value cache source_tbl register =
  if Option.is_some (Hashtbl.find_opt cache register) then
    Hashtbl.find cache register
  else if Option.is_some (int_of_string_opt register) then
    int_of_string register
  else
    let v = 
      match Hashtbl.find source_tbl register with
      | NonaryOp source -> get_source_value cache source_tbl source
      | UnaryOp (f, source) -> f (get_source_value cache source_tbl source)
      | BinaryOp (f, s1, s2) -> f (get_source_value cache source_tbl s1) (get_source_value cache source_tbl s2) in
    Hashtbl.replace cache register v;
    v

let part1 source_tbl =
  let cache = Hashtbl.create 0 in
  get_source_value cache source_tbl "a"

let part2 source_tbl =
  let register_a = part1 source_tbl in
  let cache = Hashtbl.create 0 in
  Hashtbl.add cache "b" register_a;
  get_source_value cache source_tbl "a"

let () =
  let source_tbl = parse @@ Input.get 2015 7 in
  part1 source_tbl |> print_int;
  print_newline ();
  part2 source_tbl |> print_int;
  print_newline ()
