open! Base

(** Sequence: init, init + 1, init + 2, ... *)
let inf_sequence init = Sequence.unfold ~init ~f:(fun i -> Some (i, i + 1))

let sum_sequence = Sequence.fold ~init:0 ~f:Int.( + )
let sum_list = List.fold ~init:0 ~f:Int.( + )
