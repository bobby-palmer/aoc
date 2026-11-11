open! Base

let get_aoc_cookie () =
  let aoc_env_name = "AOC_COOKIE" in
  Sys.getenv aoc_env_name
  |> Or_error.of_option
       ~error:
         (Error.create_s [%message "aoc cookie not set in env" (aoc_env_name : string)])
;;

let download ~year ~day =
  let%bind.Or_error cookie = get_aoc_cookie () in
  let url = [%string "https://adventofcode.com/%{year#Int}/day/%{day#Int}/input"] in
  let session = [%string "session=%{cookie}"] in
  let ic =
    Unix.open_process_args_in
      "curl"
      [| "curl"; "--fail"; "--silent"; "--show-error"; "-b"; session; url |]
  in
  let body = Stdio.In_channel.input_all ic in
  match Unix.close_process_in ic with
  | WEXITED 0 -> Ok body
  | _ -> Or_error.error_s [%message "curl failed"]
;;

let get ~year ~day =
  let filename =
    Stdlib.Filename.concat
      (Stdlib.Filename.get_temp_dir_name ())
      [%string "aoc_%{year#Int}_%{day#Int}.txt"]
  in
  match Stdlib.Sys.file_exists filename with
  | true -> Ok (Stdio.In_channel.read_all filename)
  | false ->
    let%map.Or_error input = download ~year ~day in
    Stdio.Out_channel.write_all filename ~data:input;
    input
;;
