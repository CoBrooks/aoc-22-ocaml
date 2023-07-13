open Advent.Input
open Advent.List
open Stdlib.List

let input = read_input "inputs/day3.full"

let explode string =
  let rec exp string acc =
    if String.length string == length acc then acc
    else
      let ch = String.get string (length acc) in
      exp string (ch :: acc)
  in
  rev (exp string [])

let compartments input =
  let parse_rucksack line =
    let chars = explode line in
    let a, b = bifurcate chars in
    (a, b)
  in
  map parse_rucksack input

let rucksacks input = map explode input

let () =
  let priority c =
    match c with
    | 'a' .. 'z' -> Char.code c - 96
    | 'A' .. 'Z' -> Char.code c - 38
    | _ -> failwith "unreachable"
  in
  let part1 =
    compartments input
    |> map (fun c -> union (fst c) (snd c))
    |> flatten |> map priority |> sum
  in
  let part2 =
    rucksacks input |> groups 3 |> map unions |> flatten |> map priority |> sum
  in
  Printf.printf "part1: %d\n" part1;
  Printf.printf "part2: %d\n" part2
