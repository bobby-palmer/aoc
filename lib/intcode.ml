type t = {
  pc: int; (* Program counter *)
  memory: int array;
  inputs: int list;
  outputs: int list;
}

(** Construct a new computer state *)
let init program =
  {
    pc = 0;
    memory = Array.copy program;
    inputs = [];
    outputs = [];
  }

(** Return true if computer has finished executing *)
let is_halted computer = computer.memory.(computer.pc) = 99

let rec get_digit num idx =
  if idx = 0 then num mod 10
  else get_digit (num / 10) (idx - 1)

let read_with_mode computer arg_idx =
  let modes = computer.memory.(computer.pc) / 100 in
  let mode = get_digit modes arg_idx in
  let immediate = computer.memory.(computer.pc + 1 + arg_idx) in
  match mode with
  | 0 -> computer.memory.(immediate)
  | 1 -> immediate
  | _ -> failwith "Bad addr mode"

let write_with_mode computer arg_idx value =
  let dst = computer.memory.(computer.pc + 1 + arg_idx) in
  computer.memory.(dst) <- value

(* Opcode impls *)

let exec_add computer =
  let a = read_with_mode computer 0
  and b = read_with_mode computer 1
  in write_with_mode computer 2 (a + b);
  { computer with pc = computer.pc + 4 }

let exec_mult computer =
  let a = read_with_mode computer 0
  and b = read_with_mode computer 1
  in write_with_mode computer 2 (a * b);
  { computer with pc = computer.pc + 4 }

let exec_input computer =
  match computer.inputs with
  | x :: xs ->
      write_with_mode computer 0 x;
      { computer with pc = computer.pc + 2; inputs = xs }
  | _ -> failwith "No pending inputs"

let exec_output computer =
  let out = read_with_mode computer 0 in
  { computer with 
    pc = computer.pc + 2; 
    outputs = List.append computer.outputs [out] 
  }

(* Generic dispatch *)

(** Execute one instruction and return the resulting computer state *)
let execute_one computer =
  let opcode = computer.memory.(computer.pc) mod 100 in
  let operator =
    match opcode with
    | 1 -> exec_add
    | 2 -> exec_mult
    | 3 -> exec_input
    | 4 -> exec_output
    | _ -> failwith "Bad opcode"
  in operator computer

(** call execute_one until computer is halted *)
let rec execute_until_done computer =
  if is_halted computer then computer
  else execute_until_done (execute_one computer)
