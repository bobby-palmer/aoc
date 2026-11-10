open! Base


let () = 
  match Sys.get_argv () with
  | [|_; year; day|] -> 
      let year = Int.of_string year in
      let day = Int.of_string day in
      let input = Aoc.Input.get ~year ~day
        |> Or_error.ok_exn
      in 
      Stdio.print_endline input
  | _ -> raise_s [%message "expected 'aoc YEAR DAY'"]
