open Aoc

type instr = Acc | Jmp | Nop

let instr_of_str = function
  | "acc" -> Acc
  | "jmp" -> Jmp
  | "nop" -> Nop
  | _ -> failwith "bad instr"

let parse_line s = Scanf.sscanf s "%s %d" (fun i num -> (instr_of_str i, num))

let instructions =
  Input.get ~year:2020 ~day:8
  |> String.split_on_char '\n'
  |> List.map parse_line
  |> Array.of_list

type computer = {
  acc: int;
  pc: int;
}

let apply computer (inst, num) =
  match inst with
  | Acc -> {acc = computer.acc + num; pc = computer.pc + 1}
  | Jmp -> {acc = computer.acc; pc = computer.pc + num}
  | Nop -> {acc = computer.acc; pc = computer.pc + 1 }

let run_to_finish instructions =
  let computer = { acc = 0; pc = 0 } in
  let visited = Hashtbl.create 16 in
  let rec aux computer =
    if (
      Hashtbl.mem visited computer.pc
      || computer.pc >= Array.length instructions
    ) then
      computer
    else (
      Hashtbl.add visited computer.pc ();
      let instruction = instructions.(computer.pc) in
      aux (apply computer instruction)
    )
  in
  aux computer

let () =
  let c = run_to_finish instructions in
  Printf.printf "Part1: %d\n" c.acc

let swap instructions =
  let valid_swaps =
    instructions
    |> Array.to_seqi
    |> Seq.filter_map (fun (i, (inst, num)) ->
        match inst with
        | Acc -> None
        | Jmp -> 
            let swapped = Array.copy instructions in
            swapped.(i) <- (Nop, num);
            Some swapped
        | Nop ->
            let swapped = Array.copy instructions in
            swapped.(i) <- (Jmp, num);
            Some swapped
    )
    |> Seq.map run_to_finish
    |> Seq.filter (fun computer ->
        computer.pc >= Array.length instructions
    )
    |> Seq.map (fun computer -> computer.acc) in
  match valid_swaps () with
  | Seq.Cons (x, _) -> x
  | _ -> failwith "no valid swaps"

let () =
  instructions
  |> swap
  |> Printf.printf "Part2: %d\n"
