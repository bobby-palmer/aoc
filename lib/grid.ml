let of_string s =
  s
  |> String.split_on_char '\n'
  |> List.to_seq
  |> Seq.mapi (fun r row ->
      row
      |> String.to_seq
      |> Seq.mapi (fun c letter ->
          ((r, c), letter)
      )
  )
  |> Seq.concat
  |> Hashtbl.of_seq

let map f g =
  g
  |> Hashtbl.to_seq
  |> Seq.map (fun (k, v) -> (k, f v))
  |> Hashtbl.of_seq

let squares g = Hashtbl.to_seq g |> Seq.map fst
