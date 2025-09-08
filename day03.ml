let input =
  In_channel.input_all In_channel.stdin

let part1 =
  let rec aux index =
    if index >= String.length input then 0
    else if input.[index] != 'm' then aux (index + 1)
    else
      try
        let end_index = String.index_from input index ')' in
        let res = Scanf.sscanf (String.sub input index (end_index + 1 - index)) "mul(%d,%d)" (fun a b -> a * b) in
        res + aux (end_index + 1)
      with _ -> aux (index + 1)
  in
  aux 0

let () =
  Printf.printf "P1: %d" part1
