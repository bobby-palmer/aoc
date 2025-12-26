open Aoc

let parse_line s =
  Scanf.sscanf s 
    "%s can fly %d km/s for %d seconds, but then must rest for %d seconds."
    (fun name speed speed_time rest -> (name, speed, speed_time, rest))

let calc_distance speed speed_time rest_time time =
  let cycle_time = speed_time + rest_time in
  let num_cycles = time / cycle_time in
  let remainder_time = time - num_cycles * cycle_time in
  let extra_speed_time = min remainder_time speed_time in
  num_cycles * speed_time * speed + extra_speed_time * speed

let parse input =
  input
  |> String.trim
  |> String.split_on_char '\n'
  |> List.map parse_line

let part1 time input =
  input
  |> parse
  |> List.map (fun (_, speed, speed_time, rest) ->
    calc_distance speed speed_time rest time
  )
  |> List.fold_left max (-1)

let range n =
  Seq.unfold (fun current ->
    if current < n then Some (current, current + 1)
    else None
  ) 0

let part2 time input =
  let parsed =
    input
    |> parse
  in
  let get_leader t =
    parsed
    |> List.map (fun (_, speed, speed_time, rest) ->
      calc_distance speed speed_time rest t
    )
    |> List.fold_left max (-1)
  in
  parsed
  |> List.map (fun (_, speed, speed_time, rest) ->
    range time
    |> Seq.map (fun t -> t + 1)
    |> Seq.filter (fun t -> calc_distance speed speed_time rest t = get_leader t)
    |> Seq.length
  )
  |> List.fold_left max (-1)

let () =
  let input = Input.get 2015 14 in
  let time = 2503 in
  part1 time input |> print_int;
  print_newline ();
  part2 time input |> print_int;
  print_newline ()
