open! Aoc
open! Core

module Box = struct
  type t = int * int * int [@@deriving compare, equal]

  let of_string s = Scanf.sscanf s "%d,%d,%d" (fun a b c -> a, b, c)

  let dist2 (x, y, z) (u, v, w) =
    Int.pow (x - u) 2 + Int.pow (y - v) 2 + Int.pow (z - w) 2
  ;;
end

let boxes =
  Input.get ~year:2025 ~day:8
  |> Or_error.ok_exn
  |> String.split_lines
  |> List.map ~f:Box.of_string
;;

let rec list2 = function
  | x :: xs -> List.map xs ~f:(fun y -> x, y) @ list2 xs
  | [] -> []
;;

let part1 =
  let boxes = List.mapi boxes ~f:(fun i box -> Union_find.create i, box) in
  let joiners =
    list2 boxes
    |> List.map ~f:(fun ((uf1, box1), (uf2, box2)) -> Box.dist2 box1 box2, (uf1, uf2))
    |> List.sort ~compare:(Comparable.lift ~f:fst Int.compare)
    |> List.map ~f:snd
  in
  let () =
    Sequence.of_list joiners
    |> Fn.flip Sequence.take 1000
    |> Sequence.iter ~f:(fun (a, b) -> Union_find.union a b)
  in
  let top3 =
    List.Assoc.sort_and_group
      boxes
      ~compare:(Comparable.lift ~f:Union_find.get Int.compare)
    |> List.map ~f:(fun (_, lst) -> List.length lst)
    |> List.sort ~compare:(Comparable.reverse compare)
    |> Fn.flip List.take 3
  in
  List.fold top3 ~init:1 ~f:Int.( * )
;;

let () = printf "Part1: %d\n" part1

let part2 =
  let boxes = List.mapi boxes ~f:(fun i box -> Union_find.create i, box) in
  let pair_dist ((_, box1), (_, box2)) = Box.dist2 box1 box2 in
  let joiners =
    list2 boxes |> List.sort ~compare:(Comparable.lift ~f:pair_dist Int.compare)
  in
  let box1, box2 =
    Sequence.of_list joiners
    |> Sequence.filter ~f:(fun ((a, _), (b, _)) -> not (Union_find.same_class a b))
    |> Sequence.fold ~init:None ~f:(fun _ ((x, b1), (y, b2)) ->
      Union_find.union x y;
      Some (b1, b2))
    |> Option.value_exn
  in
  let x1, _, _ = box1 in
  let x2, _, _ = box2 in
  x1 * x2
;;

let () = printf "Part2: %d\n" part2
