open Advent
open Advent.Input
open Advent.List
open Stdlib.List

let input = read_input "inputs/day2.full"

let input_to_tuples input =
  let lines = map (String.split_on_char ' ') input in
  let to_tuple list = (nth list 0, nth list 1) in
  map to_tuple lines

type move = Rock | Paper | Scissors

let strategy_guide_part1 =
  let parser = function
    | "A" | "X" -> Rock
    | "B" | "Y" -> Paper
    | "C" | "Z" -> Scissors
    | x -> failwith (Printf.sprintf "Invalid input %s" x)
  in
  let pairs = input_to_tuples input in
  map (Tuple.map parser) pairs

type outcome = Lose | Draw | Win

let move_from_outcome opp outcome =
  match (opp, outcome) with
  | Paper, Lose | Rock, Draw | Scissors, Win -> Rock
  | Scissors, Lose | Paper, Draw | Rock, Win -> Paper
  | Rock, Lose | Scissors, Draw | Paper, Win -> Scissors

let strategy_guide_part2 =
  let parser pair =
    let opp =
      match fst pair with
      | "A" -> Rock
      | "B" -> Paper
      | "C" -> Scissors
      | x -> failwith (Printf.sprintf "Invalid input %s" x)
    in
    let outcome =
      match snd pair with
      | "X" -> Lose
      | "Y" -> Draw
      | "Z" -> Win
      | x -> failwith (Printf.sprintf "Invalid input %s" x)
    in
    (opp, move_from_outcome opp outcome)
  in
  map parser (input_to_tuples input)

let rock_paper_scissors pair =
  let base = match snd pair with Rock -> 1 | Paper -> 2 | Scissors -> 3 in
  let pts =
    match pair with
    | Rock, Paper -> 6
    | Paper, Scissors -> 6
    | Scissors, Rock -> 6
    | Paper, Rock -> 0
    | Scissors, Paper -> 0
    | Rock, Scissors -> 0
    | _ -> 3
  in
  base + pts

let () =
  let part1 = sum (map rock_paper_scissors strategy_guide_part1) in
  let part2 = sum (map rock_paper_scissors strategy_guide_part2) in
  Printf.printf "part 1: %d\n" part1;
  Printf.printf "part 2: %d\n" part2
