open Aoc

let count_real_chars line =
  let trimmed = String.sub line 1 (String.length line - 2) in
  let rec count_non_escaped acc = function
    | '\\' :: '\\' :: xs -> count_non_escaped (acc + 1) xs
    | '\\' :: '\"' :: xs -> count_non_escaped (acc + 1) xs
    | '\\' :: 'x' :: _ :: _ :: xs -> count_non_escaped (acc + 1) xs 
    | _ :: xs -> count_non_escaped (acc + 1) xs
    | [] -> acc
  in 
  trimmed
  |> String.to_seq
  |> List.of_seq
  |> count_non_escaped 0

let part1 lines =
  let literal_chars =
    lines
    |> List.map (String.length)
    |> List.fold_left (+) 0
  and real_chars =
    lines
    |> List.map (count_real_chars)
    |> List.fold_left (+) 0
  in literal_chars - real_chars

let count_added_chars line = 
  let escapes = 
    line
    |> String.fold_left (fun acc letter ->
        match letter with
        | '\"' | '\\' -> (acc + 1)
        | _ -> acc
  ) 0 in
  escapes + 2

let part2 lines =
  lines
  |> List.map (count_added_chars)
  |> List.fold_left (+) 0

let () = 
  let lines = Input.get 2015 8 |> String.trim |> String.split_on_char '\n' in
  part1 lines |> print_int;
  print_newline ();
  part2 lines |> print_int;
  print_newline ()
