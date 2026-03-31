open Aoc

type direction = Left | Right

let parse line =
  Scanf.sscanf line "%c%d" (fun direction amount -> 
    match direction with
    | 'L' -> (Left, amount)
    | 'R' -> (Right, amount)
    | _ -> failwith "bad input"
  )

let directions =
  Input.get ~year:2025 ~day:1 |>
  String.split_on_char '\n' |>
  List.map parse

let apply pos (dir, amount) =
  let abs_pos =
    match dir with
    | Left -> pos - amount
    | Right -> pos + amount
  in
  let mod_pos = abs_pos mod 100 in
  if mod_pos < 0 then mod_pos + 100
  else mod_pos

let rec count_zero pos = function
  | x :: xs ->
      let is_zero =
        if pos = 0 then 1
        else 0
      in
      let next_pos = apply pos x in
      is_zero + count_zero next_pos xs
  | [] -> 0

let () =
  let zeros = count_zero 50 directions in
  Printf.printf "Part1: %d\n" zeros


(* let () = *)
(*   let zeroes = count_zero 50 directions in *)
(*   Printf.printf "Part2: %d\n" zeroes *)
