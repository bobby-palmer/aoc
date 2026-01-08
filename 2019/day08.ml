open Aoc

let input = Input.get 2019 8

(* Puzzle inputs from site *)
let width = 25
let height = 6

let num_pixels = width * height

let layers image =
  Seq.unfold (fun pos ->
    if pos >= String.length image then None
    else Some (String.sub image pos num_pixels, pos + num_pixels)
  ) 0

let count_digits s d =
  String.to_seq s
  |> Seq.filter (( = ) d)
  |> Seq.length

let str_min a b =
  if count_digits a '0' < count_digits b '0' then
    a
  else
    b

let () =
  let best_str =
    layers input
    |> Seq.fold_left (Option.merge str_min) None
    |> Option.get
  in
  let ones = count_digits best_str '1'
  and twos = count_digits best_str '2'
  in
  Printf.printf "Part1: %d\n" (ones * twos)

let print_color_at row col =
  let base = row * width + col in
  let rec aux layer =
    let offset = base + layer * num_pixels in
    match input.[offset] with
    | '2' -> aux (layer + 1)
    | '1' -> print_char '#'
    | '0' -> print_char ' '
    | _ -> failwith "Bad color"
  in
  aux 0

let () =
  Seq.ints 0
  |> Seq.take height
  |> Seq.iter (fun row ->
      Seq.ints 0
      |> Seq.take width
      |> Seq.iter (fun col ->
          print_color_at row col
      );
      print_newline ()
  )
