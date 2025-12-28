open Aoc

let parse_property s = Scanf.sscanf s "%[^:]: %d" (fun name count -> (name, count))

let given_props =
"children: 3
cats: 7
samoyeds: 2
pomeranians: 3
akitas: 0
vizslas: 0
goldfish: 5
trees: 3
cars: 2
perfumes: 1"
  |> String.split_on_char '\n'
  |> List.map parse_property

let parse_sue s =
  let after_name = String.index_from s 0 ':' in
  let start_props = after_name + 2 in
  let name_str = String.sub s 0 after_name in
  let props_str = String.sub s start_props (String.length s - start_props) in
  let sue_number = Scanf.sscanf name_str "Sue %d" Fun.id in
  let props = props_str
    |> String.split_on_char ','
    |> List.map (String.trim)
    |> List.map parse_property
  in
  (sue_number, props)

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse_sue

let predicate_of_prop _ count = (fun x -> x = count)

let predicate_of_prop2 name count =
  match name with
  | "cats" | "trees" -> (fun x -> x > count)
  | "pomeranians" | "goldfish" -> (fun x -> x < count)
  | _ -> (fun x -> x = count)

let prop_could_be_equal predicate_maker prop_lst =
  given_props
  |> List.for_all (fun (name, count) ->
    match List.assoc_opt name prop_lst with
    | Some value -> predicate_maker name count value
    | None -> true
  )

let find_sue predicate_maker sues =
  sues
  |> List.find (fun (_, props) ->
    prop_could_be_equal predicate_maker props
  )
  |> fst

let () =
  let input = Input.get 2015 16 in
  let sues = parse input in
  find_sue predicate_of_prop sues |> print_int;
  print_newline ();
  find_sue predicate_of_prop2 sues |> print_int;
  print_newline ()
