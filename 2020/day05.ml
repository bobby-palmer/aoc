open Aoc

let decode_binary_str s high_signal =
  s
  |> String.to_seq
  |> Seq.fold_left (fun num letter ->
        let num = num lsl 1 in
        if letter = high_signal then num lor 1 
        else num
      ) 0

let seat_of_boarding_pass bp =
  let row_str = String.sub bp 0 7 
  and col_str = String.sub bp 7 3 in
  (decode_binary_str row_str 'B', decode_binary_str col_str 'R')

let seat_id_for_pass bp =
  let (row, col) = seat_of_boarding_pass bp in
  row * 8 + col

let seat_ids = 
  Input.get ~year:2020 ~day:5
  |> String.split_on_char '\n'
  |> List.map seat_id_for_pass

let () = seat_ids |> List.fold_left max 0 |> Printf.printf "Part1: %d\n"


let () =
  let rec test seat_id =
    if seat_id > 1024 then failwith "not found"
    else if (
      List.mem (seat_id - 1) seat_ids 
      && List.mem (seat_id + 1) seat_ids
      && not (List.mem (seat_id) seat_ids)) then seat_id
    else test (seat_id + 1)
  in
  let myseat = test 0 in
  Printf.printf "Part2: %d\n" myseat
