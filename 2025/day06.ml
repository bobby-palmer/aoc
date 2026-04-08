open Aoc

let input = Input.get ~year:2025 ~day:6

let string_not_empty s = String.length s > 0

let rec transpose lst =
  if List.mem [] lst then []
  else
    let this_column = lst
      |> List.map List.hd in
    let left_over = lst
      |> List.map List.tl in 
    this_column :: transpose left_over

let vertical_problems =
  input
    |> String.split_on_char '\n'
    |> List.filter string_not_empty
    |> List.map (String.split_on_char ' ')
    |> List.map (List.filter string_not_empty)
    (* Bring operator to the top *)
    |> List.rev
    |> transpose

let solve_q = function
  | "+" :: xs -> 
      List.map int_of_string xs
        |> List.fold_left ( + ) 0
  | "*" :: xs ->
      List.map int_of_string xs
        |> List.fold_left ( * ) 1
  | _ -> failwith "Bad operator"

let () =
  let grand_total = vertical_problems
    |> List.map solve_q
    |> List.fold_left ( + ) 0
  in
  Printf.printf "Part1: %d\n" grand_total

let int_of_string s =
  s |> String.trim |> int_of_string

let parse_instruction s =
  let len = String.length s in
  if s.[len - 1] = '*' then
    (int_of_string (String.sub s 0 (len - 1)), Some '*')
  else if s.[len - 1] = '+' then
    (int_of_string (String.sub s 0 (len - 1)), Some '+')
  else
    (int_of_string s, None)


let right_to_left_problems =
  input
    |> String.split_on_char '\n'
    |> List.filter string_not_empty
    |> List.map (fun line ->
        String.to_seq line |> List.of_seq
      )
    |> transpose
    |> List.rev
    |> List.map (fun letters ->
        letters
          |> List.to_seq
          |> String.of_seq
          |> String.trim
      )
    |> List.filter string_not_empty
    |> List.map parse_instruction

let solve_right_left problems =
  let rec aux acc numbers = function
    | (number, Some '*') :: xs ->
        aux (acc + List.fold_left ( * ) number numbers) [] xs
    | (number, Some '+') :: xs ->
        aux (acc + List.fold_left ( + ) number numbers) [] xs
    | (number, None) :: xs ->
        aux acc (number :: numbers) xs
    | [] ->
        if List.length  numbers > 0 then
          failwith "excess numbers not expected"
        else
          acc
    | _ -> failwith "bad input"
  in
  aux 0 [] problems

let () =
  let grand_total = solve_right_left right_to_left_problems in
  Printf.printf "Part2: %d\n" grand_total
