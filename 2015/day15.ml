open Aoc

type ingredient = {
  capacity: int;
  durability: int;
  flavor: int;
  texture: int;
  calories: int;
}
  
let ingredient_of_string s = 
  Scanf.sscanf s 
    "%[^:]: capacity %d, durability %d, flavor %d, texture %d, calories %d"
    (fun _ capacity durability flavor texture calories ->
      {capacity; durability; flavor; texture; calories }
    )

let ingredient_mult scale ingredient =
  {
    capacity = ingredient.capacity * scale;
    durability = ingredient.durability * scale;
    flavor = ingredient.flavor * scale;
    texture = ingredient.texture * scale;
    calories = ingredient.calories * scale;
  }

let ingredient_add a b =
  {
    capacity = a.capacity + b.capacity;
    durability = a.durability + b.durability;
    flavor = a.flavor + b.flavor;
    texture = a.texture + b.texture;
    calories = a.calories + b.calories;
  }

let parse_ingredients input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map ingredient_of_string

let make_range n =
  Seq.unfold (fun current ->
    if current < n then Some (current, current + 1)
    else None
  ) 0

let rec allocate_groups amount num_groups =
  if num_groups <= 0 then failwith "num_groups must be non_zero"
  else if num_groups = 1 then 
    Seq.cons (Seq.cons amount Seq.empty) Seq.empty
  else 
    make_range (amount + 1)
    |> Seq.flat_map (fun this_allocation ->
      allocate_groups (amount - this_allocation) (num_groups - 1)
        |> Seq.map (fun xs ->
          Seq.cons this_allocation xs
        )
    )

let get_all_combos amount ingredients =
  allocate_groups amount (List.length ingredients)
  |> Seq.map (fun amounts ->
    ingredients
      |> List.to_seq
      |> Seq.map2 ingredient_mult amounts
      |> Seq.fold_left ingredient_add {
        capacity = 0;
        durability = 0;
        flavor = 0;
        texture = 0;
        calories = 0;
      }
  )

let ingredient_score ingredient =
  (max 0 ingredient.capacity) * 
  (max 0 ingredient.texture) *
  (max 0 ingredient.flavor) *
  (max 0 ingredient.durability)

let part1 ingredients =
  get_all_combos 100 ingredients
  |> Seq.map ingredient_score
  |> Seq.fold_left max 0

let part2 ingredients =
  get_all_combos 100 ingredients
  |> Seq.filter (fun i -> i.calories = 500)
  |> Seq.map ingredient_score
  |> Seq.fold_left max 0

let () =
  let input = Input.get 2015 15 in
  let ingredients = parse_ingredients input in
  part1 ingredients |> print_int;
  print_newline ();
  part2 ingredients |> print_int;
  print_newline ()
