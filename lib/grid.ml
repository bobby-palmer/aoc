type 'a t = 'a array array

let of_string s =
  s
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line ->
      String.to_seq line
      |> Array.of_seq
  )
  |> Array.of_list

let map f g = Array.map (fun row -> Array.map f row) g

let mapi f g = g |> Array.mapi (fun r row -> row |> Array.mapi (f r))

let get g (row, col) = g.(row).(col)

let dimensions g =
  let rows = Array.length g in
  let cols =
    if rows > 0 then
      Array.length (g.(0))
    else
      0
  in
  (rows, cols)

let get_opt g (row, col) =
  let (mr, mc) = dimensions g in
  if 0 <= row && row < mr && 0 <= col && col < mc then
    Some (g.(row).(col))
  else
    None
