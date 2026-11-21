open! Aoc
open! Core

module Machine = struct
  (* Remove the first and last letter of a string*)
  let trim s = String.slice s 1 (String.length s - 1)

  let parse_indicators s =
    trim s
    |> String.to_list
    |> List.filter_mapi ~f:(fun i letter ->
      match letter with
      | '#' -> Some i
      | _ -> None)
  ;;

  let parse_button s =
    trim s
    |> String.split ~on:','
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  ;;

  let parse_volts s =
    trim s
    |> String.split ~on:','
    |> List.filter ~f:(Fn.non String.is_empty)
    |> List.map ~f:Int.of_string
  ;;

  type t =
    { indicators : int list
    ; buttons : int list list
    ; volts : int list
    }

  let parse s =
    let items = String.split s ~on:' ' |> List.filter ~f:(Fn.non String.is_empty) in
    let indicators, items =
      match items with
      | x :: xs -> parse_indicators x, xs
      | _ -> raise_s [%message "expected more items in string"]
    in
    let volts, items =
      match List.rev items with
      | x :: xs -> parse_volts x, List.rev xs
      | _ -> raise_s [%message "expected more items in string"]
    in
    let buttons = List.map items ~f:parse_button in
    { indicators; buttons; volts }
  ;;
end

let machines =
  Input.get_exn ~year:2025 ~day:10 |> String.split_lines |> List.map ~f:Machine.parse
;;

let bitset =
  List.fold ~init:0 ~f:(fun acc elt ->
    let bit = 1 lsl elt in
    acc lor bit)
;;

let part1 =
  let pushes_needed { Machine.indicators; buttons; _ } =
    let indicators = bitset indicators in
    let buttons = List.map ~f:bitset buttons in
    let costs =
      List.fold
        buttons
        ~init:[ 0, 0 ]
        ~f:(fun costs button ->
          let cost_with_this =
            List.map costs ~f:(fun (value, cost) -> value lxor button, cost + 1)
          in
          List.Assoc.sort_and_group (costs @ cost_with_this) ~compare
          |> List.Assoc.map ~f:(fun costs ->
            List.min_elt costs ~compare |> Option.value_exn))
    in
    List.Assoc.find_exn costs indicators ~equal
  in
  List.map machines ~f:pushes_needed |> List.fold ~init:0 ~f:Int.( + )
;;

(* Solve like a matrix *)
let _part2 = failwith "Not done yet"
let () = printf "Part1: %d\n" part1
