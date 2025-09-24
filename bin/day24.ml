open Aoc

type operation = And | Or | Xor

type node = {
  op: operation;
  lhs: string;
  rhs: string;
  output: string;
}

let node_of_string str =
  let (lhs, op, rhs, output) = 
    Scanf.sscanf str "%s %s %s -> %s" (fun a b c d -> (a, b, c, d)) in
  let op = match op with
    | "AND" -> And
    | "OR" -> Or
    | "XOR" -> Xor
    | _ -> failwith "Bad input"
  in
  { op; lhs; rhs; output; }
  
let assign_of_string str =
  Scanf.sscanf str "%[^:]: %d" (fun a b -> (a, b))

let list_split_on_pred pred lst =
  let rec aux acc = function
    | x :: xs when pred x -> (List.rev acc, xs)
    | x :: xs -> aux (x :: acc) xs
    | _ -> failwith "Bad input"
  in
  aux [] lst

let parse input =
  let (init, operations) = 
    input 
      |> String.trim 
      |> String.split_on_char '\n' 
      |> list_split_on_pred (fun s -> String.length s = 0)
  in
  let tbl = Hashtbl.create 0 in
  Hashtbl.add_seq tbl (init |> List.to_seq |> Seq.map (assign_of_string));
  (tbl, operations |> List.map (node_of_string))

let run_to_completion (tbl, ops) =
  let find_opt = Hashtbl.find_opt tbl in
  let rec aux = function
    | x :: xs -> (
        match (find_opt x.lhs, find_opt x.rhs, find_opt x.output) with
          | (Some lhs, Some rhs, None) ->
              let result = match x.op with
                | And -> lhs land rhs
                | Or -> lhs lor rhs
                | Xor -> lhs lxor rhs
              in
              Hashtbl.add tbl x.output result;
              aux ops
          | _ -> aux xs
    )
    | [] -> tbl
  in aux ops

let bit_of_string str =
  int_of_string (String.sub str 1 (String.length str - 1))

let tbl_get_bits letter tbl =
  tbl
    |> Hashtbl.to_seq
    |> Seq.filter (fun (k, _) -> String.starts_with ~prefix:letter k)
    |> Seq.map (fun (k, v) -> (bit_of_string k, v))
    |> Seq.fold_left (fun acc (b, v) -> acc lor (v lsl b)) 0

let part1 input =
  input
    |> parse
    |> run_to_completion
    |> tbl_get_bits "z"

let part2 input =
  let (tbl, ops) = input |> parse in
  let x = tbl |> tbl_get_bits "x"
  and y = tbl |> tbl_get_bits "y" in
  let goal = x + y in

let input = Input.get_input 24

let () = print_int (part1 input)
let () = print_newline ()
