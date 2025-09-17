open Aoc

let input = Input.get_input 17

type computer = {
  pc: int;
  a: int;
  b: int;
  c: int;
  program: int Array.t;
}

let string_remove_key str =
  let idx = String.index str ':' |> (+) 2 in
  String.sub str idx (String.length str - idx)

let computer_of_string input = 
  let lines = input |> String.trim |> String.split_on_char '\n' 
    |> List.filter (fun str -> String.length str > 0)
    |> List.map (string_remove_key) in
  match lines with
    | a :: b :: c :: program :: [] ->
        {
          pc = 0; 
          a = int_of_string a; 
          b = int_of_string b; 
          c = int_of_string c; 
          program = program |> String.split_on_char ',' |> Array.of_list |> Array.map (int_of_string);
        }
    | _ -> failwith "Bad input"

let execute_one computer =
  let code = computer.program.(computer.pc) 
  and operand = computer.program.(computer.pc + 1) in
  let combo_operand () =
    match operand with
      | x when 0 <= x && x <= 3 -> x
      | 4 -> computer.a
      | 5 -> computer.b
      | 6 -> computer.c
      | _ -> failwith "Bad operand"
  in
  match code with
    | 0 -> (None, {computer with 
            pc = computer.pc + 2;
            a = computer.a / (1 lsl (combo_operand ()))})
    | 1 -> (None, {computer with
            pc = computer.pc + 2;
            b = computer.b lxor operand})
    | 2 -> (None, {computer with
            pc = computer.pc + 2;
            b = combo_operand () mod 8})
    | 3 -> 
        if computer.a = 0 then 
          (None, {computer with pc = computer.pc + 2})
        else                   
          (None, {computer with pc = operand})
    | 4 -> (None, {computer with
            pc = computer.pc + 2;
            b = computer.b lxor computer.c})
    | 5 -> (Some (combo_operand () mod 8), 
            {computer with
            pc = computer.pc + 2})
    | 6 -> (None, {computer with
            pc = computer.pc + 2;
            b = computer.a / (1 lsl (combo_operand ()))})
    | 7 -> (None, {computer with
            pc = computer.pc + 2;
            c = computer.a / (1 lsl (combo_operand ()))})
    | _ -> failwith "Bad opcode"

let is_halted computer =
  computer.pc < 0 || computer.pc >= Array.length computer.program - 1

let part1 input =
  input |> computer_of_string 
    |> Seq.unfold (fun c -> if is_halted c then None else Some (execute_one c))
    |> Seq.filter_map (fun x -> x) |> Seq.map (string_of_int) |> List.of_seq 
    |> String.concat ","

let () = part1 input |> print_endline
