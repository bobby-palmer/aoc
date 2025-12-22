let session_cookie =
  match Sys.getenv_opt "AOC_COOKIE" with
    | Some cookie -> cookie
    | None -> failwith "Please set AOC_COOKIE to download inputs!"

let download year day =
  let url = Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day in
  let command = Printf.sprintf "curl -b \"session=%s\" %s" session_cookie url in
  let ic = Unix.open_process_in command in 
  let output = In_channel.input_all ic in
  let exit_status = Unix.close_process_in ic in
  if exit_status = (Unix.WEXITED 0) then 
    output
  else (
    print_string output;
    failwith "Failed to download input"
  )
  

let get year day =
  let filename = Printf.sprintf "_input/%d_%d.txt" year day in
  if Sys.file_exists filename then
    In_channel.with_open_text filename In_channel.input_all
  else
    let input = download year day in 
    Out_channel.with_open_text filename (fun oc ->
        Out_channel.output_string oc input
      );
    input
