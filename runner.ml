let year = 2025

(*Store AOC cookies in .aoc_cookie without adding newlines*)
let cookie = 
  let ic = open_in ".aoc_cookie" in
  let cookie = input_line ic in
  close_in ic;
  cookie

(*Download the string input for a given day*)
let download_input day =
  let url = Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day in
  let cmd = Printf.sprintf "curl --cookie %s %s" (Filename.quote cookie) (Filename.quote url) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try
    while true do
      Buffer.add_channel buf ic 1024
    done
  with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buf

(*
Check if input already downloaded
If not:
  fetch it and store in file
Otherwise:
  read it from file
*)
let get_input day =
  let filename = Printf.sprintf ".inputs/%d.txt" day in
  if Sys.file_exists filename then
    open_in filename |> In_channel.input_all
  else
    let input = download_input day in
    let oc = open_out filename in
    output_string oc input;
    close_out oc;
    input

let () =
  let day = int_of_string Sys.argv.(1) in
  let input = get_input day in
  let (part1, part2) = input |> match day with
    | _ -> failwith "Invalid day" in
  Printf.printf "Part1: %d, Part2: %d" part1 part2
