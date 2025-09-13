let input = Aoc.Input.get_input 9

type block =
  | Free of int
  | Taken of int * int

let int_of_digit ch =
  if '0' <= ch && ch <= '9' then (int_of_char ch - int_of_char '0')
  else failwith "Bad digit"

let parse input =
  input |> String.trim |> String.to_seq |> Seq.mapi (fun idx letter ->
    let length = int_of_digit letter in
    if idx mod 2 == 0 then
      Taken (length, idx / 2)
    else
      Free length
  )

let mk_fs block_list =
  let aux = function
    | Free len ->
        if len == 0 then None
        else Some (Free 1, Free (len - 1))
    | Taken (len, id) ->
        if len == 0 then None
        else Some (Taken (1, id), Taken (len - 1, id))
  in block_list |> Seq.flat_map (fun elt -> elt |> Seq.unfold aux)

let part1 input =
  let fs = input |> parse |> mk_fs |> Array.of_seq in
  let rec aux l r acc =
    if l >= r then acc
    else (
      match fs.(l) with
        | Free _ -> (
            match fs.(r - 1) with
              | Free _ -> aux l (r - 1) acc
              | Taken (_, id) -> aux (l + 1) (r - 1) (acc + l * id)
          )
        | Taken (_, id) -> aux (l + 1) r (acc + l * id)
    )
  in aux 0 (Array.length fs) 0

let rec ins_end lst required_len id =
  match lst with
    | [] -> (false, [])
    | x :: xs ->
        let (inserted, xs) = ins_end xs required_len id in
        if inserted then (true, x :: xs)
        else (
          match x with
            | Free len when len >= required_len -> (true, Free (len - required_len) :: (
                Taken (required_len, id) :: xs
              ))
            | _ -> (false, x :: xs)
        )

let rec move_block = function
  | [] -> []
  | x :: xs ->
      match x with
        | Free _ -> x :: move_block xs
        | Taken (len, id) ->
            let (inserted, xs) = ins_end xs len id in
            let head = if inserted then Free len else Taken (len, id) in
            head :: move_block xs
    
(*This is slow asfffff*)
let part2 input =
  input |> parse |> List.of_seq |> List.rev |> move_block 
    |> List.rev |> List.to_seq |> mk_fs |> Seq.fold_lefti (fun acc idx elt ->
        match elt with
          | Free _ -> acc
          | Taken (_, id) -> acc + idx * id
      ) 0

let () =
  Printf.printf "%d, %d" (part1 input) (part2 input)
