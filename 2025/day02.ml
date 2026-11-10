open! Base

let parse input = 
  String.strip input
  |> String.split ~on:','
  |> List.map ~f:(fun s -> 
      Stdlib.Scanf.sscanf s "%d-%d" (fun a b -> (a, b)))

let ranges =
  Aoc.Input.get ~year:2025 ~day:2
  |> Or_error.ok_exn
  |> parse

let rec count_digits num =
  match num with
  | 0 -> 0
  | num -> 1 + count_digits (num / 10)

(* [repeat num ~n] returns a number that is num concatenated n times *)
let repeat num =
  let digits = count_digits num in
  let append_num acc =
    acc * (10 ** digits) + num
  in
  Fn.apply_n_times append_num 0

let rec repeats_in_range ~n (a, b) =
  match a < b with
  | false -> repeats_in_range ~n (b, a)
  | true ->
      Aoc.Util.inf_sequence 0
      |> Sequence.map ~f:(repeat ~n)
      |> Sequence.drop_while ~f:((>) a)
      |> Sequence.take_while ~f:((>=) b)

let part1 ranges =
  List.map ranges ~f:(repeats_in_range ~n:2)
  |> List.map ~f:(Aoc.Util.sum_sequence)
  |> Aoc.Util.sum_list

let part2 ranges =
  List.map ranges ~f:(fun (a, b) ->
    (* Most repeats possible in the range. e.g. range ending in 111 has max n =
      3 *)
    let max_repeats = count_digits (max a b) in
    Sequence.range 2 max_repeats ~stop:`inclusive
    |> Sequence.concat_map ~f:(fun n -> repeats_in_range ~n (a, b))
    (* Dedup cases where a number can be split into different repeats. 
       e.g. 1111 -> [1; 1; 1; 1] or [11; 11] *)
    |> Set.of_sequence (module Int)
    |> Set.fold ~init:0 ~f:Int.(+)
  )
  |> Aoc.Util.sum_list

let () =
  Stdio.printf "Part1: %d\n" (part1 ranges);
  Stdio.printf "Part2: %d\n" (part2 ranges)
