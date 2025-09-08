let input =
  In_channel.input_all In_channel.stdin

let ints_of_string s =
  let buf = Scanf.Scanning.from_string s in
  let rec aux acc =
    try
      let n = Scanf.bscanf buf " %d" (fun x -> x) in
      aux (n :: acc)
    with
    | End_of_file -> List.rev acc
    | Scanf.Scan_failure _ -> List.rev acc
  in
  aux []

let rec is_true lst pred =
  match lst with
    | [] | [_] -> true
    | x :: y :: rest ->
        if pred x y then is_true (y :: rest) pred
        else false

let increasing x y =
  x < y &&
  y - x <= 3 &&
  y - x >= 1

let decreasing x y =
  x > y &&
  x - y <= 3 &&
  x - y >= 1

let part1 input =
  let lines = String.split_on_char '\n' input in
  let lines = List.filter (fun line -> String.length line > 0) lines in
  let rec aux lines =
    match lines with
      | [] -> 0
      | line :: rest ->
          let ints = ints_of_string line in
          let increasing = is_true ints increasing
          and decreasing = is_true ints decreasing in
          let current = if increasing || decreasing then 1 else 0 in
          current + aux rest
    in
    aux lines

let is_mostly_true lst pred =
  let rec aux lst =
    match lst with
      | [] | [_] -> true
      | x :: y :: rest ->
          (pred x y && aux (y :: rest)) ||
          is_true (x :: rest) pred
  in
  match lst with
    | [] -> true
    | x :: rest ->
        aux (x :: rest) || is_true rest pred

let part2 input = 
  let lines = String.split_on_char '\n' input in
  List.length (List.filter (fun line -> 
    match ints_of_string line with   
      | [] -> false
      | lst ->
          is_mostly_true lst increasing || is_mostly_true lst decreasing
  ) lines)
          
let () =
  let p1 = part1 input 
  and p2 = part2 input in
  Printf.printf "P1: %d, P2: %d" p1 p2
