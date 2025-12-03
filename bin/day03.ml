let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line ->
      line |> String.to_seq |> Seq.map (fun c ->
        int_of_char c - int_of_char '0'
      )
  )

(** Remove one int from the list such that it is lexicoigraphically maximized *)
let rec remove_worst = function
  | a :: b :: xs ->
      if a < b then b :: xs
      else a :: remove_worst (b :: xs)
  | _ :: [] -> []
  | _ -> failwith "Empty input"

(** 
  Given a power bank, return the max volts it can make given the number of
  batteries i can use
*)
let get_max_pwr batteries_allowed batteries =
  batteries
  |> Seq.fold_left (fun acc elt ->
        let acc = acc @ [elt] in
        if List.length acc > batteries_allowed then remove_worst acc
        else acc
    ) []
  |> List.fold_left (fun acc elt -> 10 * acc + elt) 0

(** Solve problem for k batteries allowed *)
let solve batteries_allowed input =
  input
  |> parse
  |> List.map (get_max_pwr batteries_allowed)
  |> List.fold_left (+) 0

let part1 = solve 2

let part2 = solve 12

let () =
  let input = Aoc.Input.get_input 3 in
  input |> part1 |> print_int;
  print_newline ();
  input  |> part2 |> print_int;
  print_newline ();;
