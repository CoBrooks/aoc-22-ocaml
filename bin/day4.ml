open Advent.Input
open Stdlib.List

let input = read_input "inputs/day4.full"

let sections input =
  let pairs = String.split_on_char ',' in
  let ranges s =
    s |> String.split_on_char '-' |> map int_of_string |> function
    | [ first; last ] -> (first, last)
    | _ -> failwith "unreachable"
    (*| [ first; last ] -> range_inc first last *)
  in
  input |> map pairs |> map (map ranges)

let fully_overlapping_sections =
  filter (function
    | [ (a1, a2); (b1, b2) ] -> (a1 <= b1 && a2 >= b2) || (b1 <= a1 && b2 >= a2)
    | _ -> failwith "unreachable")

let overlapping_sections =
  filter (function
    | [ (a1, a2); (b1, b2) ] -> (a2 >= b1 && a1 <= b1) || (b2 >= a1 && b1 <= a1)
    | _ -> failwith "unreachable")

let () =
  let part1 = sections input |> fully_overlapping_sections |> length in
  let part2 = sections input |> overlapping_sections |> length in
  Printf.printf "part1: %d\n" part1;
  Printf.printf "part2: %d\n" part2
