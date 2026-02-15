type 'a t = 'a array array

let of_string s =
  s
  |> String.split_on_char '\n'
  |> List.map (fun row -> 
        row
        |> String.to_seq
        |> Array.of_seq
    )
  |> Array.of_list

let rows g = Array.length g

let cols g =
  if rows g > 0 then 
    Array.length (Array.get g 0)
  else 0

let get g row col =
  g.(row).(col)
