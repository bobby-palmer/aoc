let input = 36000000

let range lower upper =
  Seq.unfold (fun current ->
    if current < upper then Some (current, current + 1)
    else None
  ) lower

let factor_pairs number =
  range 1 (1 + (int_of_float @@ sqrt @@ float_of_int number))
  |> Seq.filter (fun f -> number mod f = 0)
  |> Seq.map (fun f -> (f, number / f))

let presents_recieved house_number =
  factor_pairs house_number
  |> Seq.fold_left (fun acc (x, y) ->
      acc + 10 * x + 10 * y
  ) 0

let part1 =
  range 1 input
  |> Seq.find (fun house_number ->
      presents_recieved house_number >= input
  )
  |> Option.get

(* Hella slow, fix this later *)
let () = print_int part1; print_newline ()
