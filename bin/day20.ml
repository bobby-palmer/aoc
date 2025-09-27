type pos_type = Track | Wall | Start | End

let pos_type_of_char = function
  | 'S' -> Start
  | 'E' -> End
  | '.' -> Track
  | '#' -> Wall
  | _ -> failwith "Invalid letter"

module Ord = struct
  type t = int * int
  let compare = compare
end

module PosMap = Map.Make(Ord)

let parse input =
  input 
  |> String.trim 
  |> String.split_on_char '\n'
  |> List.to_seq
  |> Seq.mapi (
    fun row letters ->
      letters 
        |> String.to_seq
        |> Seq.mapi (
          fun col letter ->
            ((row, col), pos_type_of_char letter)
        )
  )
  |> Seq.concat
  |> PosMap.of_seq

module PriorityQueue = struct
  
  let init () = []

  let rec push pq (prio, elt) =
    match pq with
      | [] -> [(prio, elt)]
      | (tp, te) :: xs ->
          if tp <= prio then (prio, elt) :: (tp, te) :: xs
          else (tp, te) :: push xs (prio, elt)

  let top = function
    | (prio, elt) :: _ -> (prio, elt)
    | _ -> failwith "Empty queue"

  let pop = function
    | _ :: xs -> xs
    | _ -> failwith "Empty queue"

end
