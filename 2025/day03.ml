open! Base

let parse input =
  let parse_bank s =
    String.to_list s |> List.map ~f:(fun digit -> Char.to_int digit - Char.to_int '0')
  in
  String.strip input |> String.split_lines |> List.map ~f:parse_bank
;;

let banks = Aoc.Input.get ~year:2025 ~day:3 |> Or_error.ok_exn |> parse

let voltage_for_bank bank ~n =
  let rec voltages_for_bank ~n =
    match n with
    | 1 -> bank
    | _ ->
      let max_volts_before =
        voltages_for_bank ~n:(n - 1)
        |> List.folding_map ~init:0 ~f:(fun best_so_far current_voltage ->
          max best_so_far current_voltage, best_so_far)
      in
      List.zip_exn max_volts_before bank
      |> List.map ~f:(fun (max_volt_before, current_volt) ->
        (10 * max_volt_before) + current_volt)
  in
  voltages_for_bank ~n |> List.fold ~init:0 ~f:Int.max
;;

let part1 banks = List.map banks ~f:(voltage_for_bank ~n:2) |> Aoc.Util.sum_list
let part2 banks = List.map banks ~f:(voltage_for_bank ~n:12) |> Aoc.Util.sum_list

let () =
  Stdio.printf "Part1: %d\n" (part1 banks);
  Stdio.printf "Part2: %d\n" (part2 banks)
;;
