open Aoc

let parse_bag s =
  let (color, xs) = 
    Scanf.sscanf s "%s %s bags contain%[^.]" (fun c1 c2 xs -> ((c1, c2), xs))
  in
  let parse_contain s = 
    Scanf.sscanf s " %d %s %s bag" (fun count c1 c2 -> ((c1, c2), count)) 
  in
  let contains =
    if xs = " no other bags" then []
    else
      xs
      |> String.split_on_char ','
      |> List.map parse_contain
  in
  (color, contains)

let input =
  Input.get ~year:2020 ~day:7
  |> String.split_on_char '\n'
  |> List.map parse_bag

let dfs_contains graph =
  let tbl = Hashtbl.create 16 in
  let rec aux node contains =
    if Hashtbl.mem tbl node then 
      Hashtbl.find tbl node
    else if List.assoc node graph |> List.assoc_opt contains |> Option.is_some then 
      true
    else (
      Hashtbl.replace tbl node false;
      let sub_node_result =
        List.assoc node graph
        |> List.map (fun (color, _) -> aux color contains)
        |> List.fold_left ( || ) false
      in
      Hashtbl.replace tbl node sub_node_result;
      sub_node_result
    )
  in
  aux

let () =
  let dfser = dfs_contains input in
  let contains = ("shiny", "gold") in
  let ans =
    input
    |> List.filter (fun (c, _) -> dfser c contains)
    |> List.length
  in
  Printf.printf "Part1: %d\n" ans

let count_contains graph =
  let tbl = Hashtbl.create 16 in
  let rec aux node =
    if Hashtbl.mem tbl node then
      Hashtbl.find tbl node
    else (
      let contained =
        List.assoc node graph
        |> List.map (fun (c, count) -> aux c * count + count)
        |> List.fold_left ( + ) 0
      in
      Hashtbl.replace tbl node contained;
      contained
    )
  in
  aux

let () =
  let container = count_contains input in
  container ("shiny", "gold") 
    |> Printf.printf "Part2: %d\n"
