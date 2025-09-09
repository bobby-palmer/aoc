(*Download the string input for a given day*)
let get_input day =
  let cookie = 
    try Sys.getenv "AOC_COOKIE" 
    with Not_found -> 
      failwith "AOC_COOKIE not set in environment"
  and
  url = Printf.sprintf "https://adventofcode.com/2024/day/%d/input" day in
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

let () =
  let day = int_of_string Sys.argv.(1) in
  let input = get_input day in
  Printf.printf "%s" input
