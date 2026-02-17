include Stdlib.String

let split_paragraphs s =
  let lines = Stdlib.String.split_on_char '\n' s in
  let rec aux acc = function
    | x :: xs when x = "" -> acc :: (aux "" xs)
    | x :: xs -> aux (acc^"\n"^x) xs
    | [] -> []
  in
  aux "" lines

