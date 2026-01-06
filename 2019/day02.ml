open Aoc

let program =
  Input.get 2019 2
  |> String.split_on_char ','
  |> List.map int_of_string
  |> Array.of_list

type computer = {
  program: int array;
  pc: int;
}

let new_computer program =
  let program = Array.copy program in
  { program = program; pc = 0 }

let computer_read computer offset =
  computer.program.(offset)

let apply_op = function
  | 1 -> ( + )
  | 2 -> ( * )
  | _ -> failwith "Bad opcode"

let execute_one computer =
  let computer_read = computer_read computer in
  let op = computer_read computer.pc in
  match op with
  | 1 | 2 ->
      let a = computer_read (computer.pc + 1) 
            |> computer_read and
          b = computer_read (computer.pc + 2) 
            |> computer_read and 
          dest = computer.program .(computer.pc + 3) 
      in computer.program.(dest) <- apply_op op a b;
      {computer with pc = computer.pc + 4}
  | _ -> failwith "Bad opcode"

let rec run_to_completion computer =
  let computer_read = computer_read computer in
  if computer_read computer.pc = 99 then
    computer_read 0
  else
    run_to_completion (execute_one computer)

let run_with arg1 arg2 =
  let computer = new_computer program in
  computer.program.(1) <- arg1;
  computer.program.(2) <- arg2;
  run_to_completion computer

let () = Printf.printf "Part1: %d\n" (run_with 12 2)

let () =
  let (arg1, arg2) =
    Seq.ints 0
    |> Seq.take 100
    |> Seq.flat_map (fun arg1 ->
        Seq.ints 0
        |> Seq.take 100
        |> Seq.map (fun arg2 -> (arg1, arg2))
    )
    |> Seq.find (fun (arg1, arg2) ->
      run_with arg1 arg2 = 19690720
    )
    |> Option.get
  in
  let answer = arg1 * 100 + arg2 in
  Printf.printf "Part2: %d\n" answer
