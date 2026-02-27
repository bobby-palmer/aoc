open Aoc

let input =
  let adapters =
    Input.get ~year:2020 ~day:10
      |> String.split_on_char '\n'
      |> List.map int_of_string
  in
  let highest = List.fold_left max 0 adapters in
  (highest + 3) :: (0 :: adapters)

let rec list_diff = function
  | a :: b :: xs ->
      (b - a) :: (list_diff (b :: xs))
  | _ -> []

let part1 input =
  let diffs =
    input
    |> List.sort compare
    |> list_diff
  in
  let ones =
    diffs
    |> List.filter (( = ) 1)
    |> List.length
  and threes =
    diffs
    |> List.filter (( = ) 3)
    |> List.length
  in
  ones * threes

let () = part1 input |> Printf.printf "Part1: %d\n"

let count_ways input =
  let tbl = Hashtbl.create 16 in
  let highest = List.fold_left max 0 input in
  let rec aux start =
    if not (List.mem start input) then 0
    else if start = highest then 1
    else if Hashtbl.mem tbl start then
      Hashtbl.find tbl start
    else
      let candiates = [start + 1; start + 2; start + 3] in
      let ways = 
        candiates
        |> List.map aux
        |> List.fold_left ( + ) 0
      in Hashtbl.add tbl start ways;
      ways
  in aux 0


let () = count_ways input |> Printf.printf "Part2: %d\n"
