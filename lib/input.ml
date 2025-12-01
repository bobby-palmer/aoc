(*Hard coding this bc idc*)
let year = 2025

(* Cookie should be stored as environment variable in order to get inputs!*)
let cookie = 
  Sys.getenv "AOC_COOKIE"

(** Download the string input for a given day *)
let download_input day =
  let url = Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day in
  let cmd = Printf.sprintf 
    "curl --cookie %s %s" (Filename.quote cookie) (Filename.quote url) in
  let ic = Unix.open_process_in cmd in
  let buf = Buffer.create 1024 in
  (try
    while true do
      Buffer.add_channel buf ic 1024
    done
  with End_of_file -> ());
  ignore (Unix.close_process_in ic);
  Buffer.contents buf

(** Check if input already downloaded
    If not, fetch it and store in file. Otherwise, read it from file *)
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
