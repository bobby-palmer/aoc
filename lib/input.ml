let get_input year day =
  let session =
    match Sys.getenv_opt "AOC_SESSION" with
    | Some s -> s
    | None -> failwith "AOC_SESSION environment variable not set"
  in
  let uri =
    Uri.of_string
      (Printf.sprintf "https://adventofcode.com/%d/day/%d/input" year day)
  in
  let headers =
    Cohttp.Header.of_list [ ("Cookie", "session=" ^ session) ]
  in
  let body =
    Lwt_main.run
      (let open Lwt.Syntax in
       let* resp, body = Cohttp_lwt_unix.Client.get ~headers uri in
       let status = Cohttp.Response.status resp in
       if Cohttp.Code.is_success (Cohttp.Code.code_of_status status) then
         Cohttp_lwt.Body.to_string body
       else
         failwith
           (Printf.sprintf "Failed to fetch input: %s"
              (Cohttp.Code.string_of_status status)))
  in
  String.trim body
