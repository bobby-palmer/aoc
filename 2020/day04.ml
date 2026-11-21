open! Aoc
open! Core

module Field = struct
  type t =
    | Birth_year
    | Issue_year
    | Expiration_year
    | Height
    | Hair_color
    | Eye_color
    | Passport_id
    | Country_id
  [@@deriving enumerate, equal]

  let of_string_exn s =
    match String.lsplit2_exn s ~on:':' with
    | "byr", value -> Birth_year, value
    | "iyr", value -> Issue_year, value
    | "eyr", value -> Expiration_year, value
    | "hgt", value -> Height, value
    | "hcl", value -> Hair_color, value
    | "ecl", value -> Eye_color, value
    | "pid", value -> Passport_id, value
    | "cid", value -> Country_id, value
    | field, _ -> raise_s [%message "bad field" (field : string)]
  ;;

  let is_required = function
    | Country_id -> false
    | _ -> true
  ;;

  let has_all_required fields =
    List.filter all ~f:is_required |> List.for_all ~f:(List.Assoc.mem fields ~equal)
  ;;

  let is_valid (field, value) =
    let is_in_range ~lower ~upper value =
      match Int.of_string_opt value with
      | Some value -> lower <= value && value <= upper
      | None -> false
    in
    match field with
    | Birth_year -> is_in_range value ~lower:1920 ~upper:2002
    | Issue_year -> is_in_range value ~lower:2010 ~upper:2020
    | Expiration_year -> is_in_range value ~lower:2020 ~upper:2030
    | Height ->
      (match
         String.chop_suffix value ~suffix:"cm", String.chop_suffix value ~suffix:"in"
       with
       | Some cm, None -> is_in_range cm ~lower:150 ~upper:193
       | None, Some inches -> is_in_range inches ~lower:59 ~upper:76
       | _, _ -> false)
    | Hair_color ->
      (match String.chop_prefix value ~prefix:"#" with
       | Some rest ->
         let is_valid letter =
           Char.(('0' <= letter && letter <= '9') || ('a' <= letter && letter <= 'f'))
         in
         String.length rest = 6 && String.for_all rest ~f:is_valid
       | None -> false)
    | Eye_color ->
      List.exists
        [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
        ~f:(String.equal value)
    | Passport_id -> String.length value = 9 && Int.of_string_opt value |> Option.is_some
    | Country_id -> true
  ;;
end

let passports =
  let parse_passport s = String.split s ~on:' ' |> List.map ~f:Field.of_string_exn in
  Input.get_exn ~year:2020 ~day:4
  |> String.split_lines
  |> List.group ~break:(fun s _ -> String.is_empty s)
  |> List.map ~f:(String.concat ~sep:" ")
  |> List.map ~f:String.strip
  |> List.map ~f:parse_passport
;;

let part1 = List.count passports ~f:Field.has_all_required

let part2 =
  List.count passports ~f:(fun passport ->
    Field.has_all_required passport && List.for_all passport ~f:Field.is_valid)
;;

let () =
  printf "Part1: %d\n" part1;
  printf "Part2: %d\n" part2
;;
