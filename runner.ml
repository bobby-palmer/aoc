(*https://adventofcode.com/2024/day/6/input*)

let () =
  let day = int_of_string Sys.argv.(1) in
  let (part1, part2) = match day with
    | _ -> failwith "Invalid day"
  in
  Printf.printf "Part1: %d, Part2: %d" part1 part2
