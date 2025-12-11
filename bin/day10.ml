type item = Goal of int list | Button of int list | Volts of int list

(** Remove first and last charater of string *)
let str_trim_ends str =
  String.sub str 1 (String.length str - 2)

(** Parse one item from a line *)
let _item_of_str str =
  match str.[0] with
  | '[' -> 
      str
      |> str_trim_ends
      |> String.to_seqi
      |> Seq.filter (fun (_, letter) -> letter = '#')
      |> Seq.map (fst)
      |> List.of_seq
      |> fun v -> Goal v

  | '(' -> Button []

  | '{' -> Volts []

  | _ -> failwith "Bad item str"
