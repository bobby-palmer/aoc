open Aoc

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map (fun line -> Scanf.sscanf line "%dx%dx%d" (fun a b c -> (a, b, c)))

let calculate_paper (w, l, h) =
  let a = w * l and b = w * h and c = l * h in
  2 * a + 2 * b + 2 * c + (min (min a b) c)

let part1 parsed =
  parsed
  |> List.map calculate_paper
  |> List.fold_left (+) 0

let cubic_feet (a, b, c) =
  a * b * c

let perimeter a b =
  2 * (a + b)

let smallest_perimeter (a, b, c) =
  let smallest_with_a = min (perimeter a b) (perimeter a c) in
  min smallest_with_a (perimeter b c)

let calculate_ribbon box =
  smallest_perimeter box + cubic_feet box

let part2 parsed =
  parsed
  |> List.map calculate_ribbon
  |> List.fold_left (+) 0

let () =
  let parsed = parse @@ Input.get 2015 2 in
  part1 parsed |> print_int;
  print_newline ();
  part2 parsed |> print_int;
  print_newline ()
