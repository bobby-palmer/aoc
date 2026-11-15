open! Core

(** Sequence: init, init + 1, init + 2, ... *)
let inf_sequence init = Sequence.unfold ~init ~f:(fun i -> Some (i, i + 1))

let sum_sequence = Sequence.fold ~init:0 ~f:Int.( + )
let sum_list = List.fold ~init:0 ~f:Int.( + )

(** sequence over all the pairs of the list. For each element [(x, y)], [index(x) < index(y)] *)
let rec pairs_of_list = function
  | x :: xs ->
    let pairs_with_x = Sequence.of_list xs |> Sequence.map ~f:(fun y -> x, y) in
    Sequence.append pairs_with_x (pairs_of_list xs)
  | [] -> Sequence.empty
;;

(** Return the two elements in order [min, max] as defined by compare *)
let min_max a b ~compare =
  match compare a b <= 0 with
  | true -> a, b
  | false -> b, a
;;
