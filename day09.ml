type block =
  | Free of int
  | Taken of int * int

let parse input = 
  input |> String.trim |> String.to_seq |> Seq.mapi (fun idx letter ->
    let length = int_of_char letter in
    if idx mod 2 == 0 then
      Taken (length, idx / 2)
    else
      Free length
  ) |> List.of_seq

let make_iter lst =
  List.to_seq lst |> Seq.flat_map (fun state ->
    let aux = function
      | Free len -> 
          if len > 0 then
            Some (Free 1, Free (len - 1))
          else
            None
      | Taken (len, id) ->
          if len > 0 then
            Some (Taken (1, id), Taken (len - 1, id))
          else
            None
    in Seq.unfold aux state
  )
