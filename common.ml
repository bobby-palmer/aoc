let grid_of_string input =
  input |> String.trim |> String.split_on_char '\n' |> Array.of_list |>
  Array.map (fun row -> row |> String.to_seq |> Array.of_seq)
