let input = "bgvyzdsv"

let md_hash_hex s num = 
  s ^ (string_of_int  num)
  |> Digest.string
  |> Digest.to_hex


let part1 input =
  Seq.unfold (fun num -> Some (num, num + 1)) 1
  |> Seq.find (fun elt ->
    String.starts_with ~prefix:"00000" (md_hash_hex input elt)
  )
  |> Option.get

let part2 input =
  Seq.unfold (fun num -> Some (num, num + 1)) 1
  |> Seq.find (fun elt ->
    String.starts_with ~prefix:"000000" (md_hash_hex input elt)
  )
  |> Option.get

let () =
  part1 input |> print_int;
  print_newline ();
  part2 input |> print_int;
  print_newline ()
