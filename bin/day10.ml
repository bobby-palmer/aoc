(** Remove one char from the begining and end of the string *)
let string_trim_ends str =
  String.sub str 1 (String.length str - 2)

let parse_line line =
  let sections = line
    |> String.split_on_char ' '
    |> List.map string_trim_ends
  in
  let goal = List.hd sections
  and rest = List.tl sections
  in
  ()

let _parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse_line
