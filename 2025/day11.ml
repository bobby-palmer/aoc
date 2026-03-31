open Aoc

let parse line =
  match String.split_on_char ' ' line with
  | x :: xs ->
      (String.sub x 0 (String.length x - 1), xs)
  | _ -> failwith "invalid string"

let graph =
  Input.get ~year:2025 ~day:11
    |> String.split_on_char '\n'
    |> List.map parse
    |> List.to_seq
    |> Hashtbl.of_seq

let opt_default v = function
  | Some x -> x
  | None -> v

let count_ways g s e =
  let cache = Hashtbl.create 16 in
  let rec aux node =
    if node = e then 1
    else if Hashtbl.mem cache node then
      Hashtbl.find cache node
    else
      let ways =
        Hashtbl.find_opt g node
          |> opt_default []
          |> List.map aux
          |> List.fold_left ( + ) 0
      in
      Hashtbl.add cache node ways;
      ways
  in
  aux s

let () =
  count_ways graph "you" "out" 
    |> Printf.printf "Part1: %d\n"

let () =
  let count_ways = count_ways graph in
  let ans = 
    (count_ways "svr" "dac") * (count_ways "dac" "fft") * (count_ways "fft" "out")
    + (count_ways "svr" "fft") * (count_ways "fft" "dac") * (count_ways "dac" "out")
  in
  Printf.printf "Part2: %d\n" ans
