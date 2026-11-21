open! Aoc
open! Core

let graph =
  let parse line =
    match String.split line ~on:':' with
    | [ x; rest ] -> x, String.split rest ~on:' '
    | _ -> failwith "Bad input"
  in
  Input.get_exn ~year:2025 ~day:11
  |> String.split_lines
  |> List.map ~f:parse
  |> String.Map.of_alist_exn
;;

let count_paths from to_ =
  let tbl = Hashtbl.create (module String) in
  let rec aux from =
    match String.equal from to_ with
    | true -> 1
    | false ->
      let compute () =
        let neighbors = Map.find graph from |> Option.value ~default:[] in
        List.map neighbors ~f:aux |> List.fold ~init:0 ~f:Int.( + )
      in
      Hashtbl.find_or_add tbl from ~default:compute
  in
  aux from
;;

let part1 = count_paths "you" "out"

let part2 =
  let start = "svr" in
  let fft = "fft" in
  let dac = "dac" in
  let out = "out" in
  (count_paths start fft * count_paths fft dac * count_paths dac out)
  + (count_paths start dac * count_paths dac fft * count_paths fft out)
;;

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;
