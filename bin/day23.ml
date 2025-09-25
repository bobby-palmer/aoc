open Aoc

let input = Input.get_input 23

let parse input =
  input 
    |> String.trim 
    |> String.split_on_char '\n' 
    |> List.map (fun line ->
          match line |> String.split_on_char '-' with
            | a :: b :: [] -> (a, b)
            | _ -> failwith "Bad line"
        )

let canonicalize lst =
  lst |> List.map (fun (a, b) ->
    if a < b then (a, b)
    else (b, a)
  )

module StrSet = Set.Make(
    struct
      type t = string
      let compare = compare
    end
  )

let graph_get graph node =
  match Hashtbl.find_opt graph node with
    | Some set -> set
    | None -> StrSet.empty

let build_adj edges =
  let tbl = Hashtbl.create 0 in
  edges |> List.iter (fun (a, b) ->
    a
      |> graph_get tbl
      |> StrSet.add b
      |> Hashtbl.replace tbl a;
    ()
  );
  tbl

let connected_trips graph =
  graph 
    |> Hashtbl.to_seq 
    |> Seq.flat_map (
      fun (a, aset) ->
        aset 
          |> StrSet.to_seq
          |> Seq.flat_map (
            fun b ->
              b 
                |> graph_get graph
                |> StrSet.inter aset
                |> StrSet.to_seq
                |> Seq.map (fun c -> (a, b, c))
          )
      )

let part1 input =
  input 
    |> parse 
    |> canonicalize 
    |> build_adj 
    |> connected_trips
    |> Seq.filter (
      fun (a, b, c) ->
        let sw = String.starts_with ~prefix:"t" in
        sw a || sw b || sw c
    )
    |> Seq.length

let () = print_int (part1 input)
let () = print_newline ()

let lst_max a b =
  if List.length a < List.length b then b
  else a

let max_component graph start_node =
  let rec aux candidates =
    candidates 
      |> StrSet.elements
      |> List.fold_left (
        fun acc candidate ->
            candidate :: aux (StrSet.inter candidates (graph_get graph candidate))
              |> lst_max acc
      ) []
  in start_node :: aux (graph_get graph start_node)

let part2 input =
  let graph = input |> parse |> canonicalize |> build_adj in
  graph 
    |> Hashtbl.to_seq_keys
    |> Seq.map (max_component graph)
    |> Seq.fold_left (lst_max) []
    |> List.sort (compare)
    |> String.concat ","

let () = print_endline (part2 input)
