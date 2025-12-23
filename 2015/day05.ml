open Aoc

let string_to_chars s =
  List.init (String.length s) (String.get s)

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map string_to_chars

let is_vowel = function
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false

let count_vowels chars =
  chars |> List.filter is_vowel |> List.length

let rec has_double_letter = function
  | a :: b :: _ when a = b -> true
  | _ :: xs -> has_double_letter xs
  | [] -> false

let rec has_naughty_word = function
  | a :: b :: xs ->
    (match (a, b) with
      | ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') -> true
      | _ -> has_naughty_word (b :: xs))
  | _ -> false

let part1 parsed =
  parsed
  |> List.filter (fun chars ->
    count_vowels chars >= 3 &&
    has_double_letter chars &&
    not (has_naughty_word chars)
  )
  |> List.length

let rec has_singles x y = function
  | a :: b :: xs ->
    if a = x && b = y then true
    else has_singles x y (b :: xs)
  | _ -> false

let rec has_doubles = function
  | a :: b :: xs ->
    if has_singles a b xs then true
    else has_doubles (b :: xs)
  | _ -> false

let rec has_one_space_repeat = function
  | a :: _ :: b :: _ when a = b -> true
  | _ :: xs -> has_one_space_repeat xs
  | [] -> false

let part2 parsed =
  parsed
  |> List.filter (fun chars ->
    has_doubles chars && has_one_space_repeat chars
  )
  |> List.length

let () = 
  let parsed = parse @@ Input.get 2015 5 in
  part1 parsed |> print_int;
  print_newline ();
  part2 parsed |> print_int
