open Advent
open List

let input = read_input "inputs/day1.full"

(** Takes the lines from the input, groups them as defined in the problem,
    and calculates their sum.
    ["234"; "14"; ""; "3"; ""] -> [248; 3]
    @param input raw lines from the AOC input *)
let groups input =
  let rec group sums current input =
    match input with
    | "" :: rest -> group (sum current :: sums) [] rest
    | next :: rest -> group sums (int_of_string next :: current) rest
    | [] -> sums
  in
  group [] [] input

(** Takes the first n elements from a list *)
let take n list =
  let rec take_inner n acc rest =
    match rest with
    | _ when n == 0 -> rev acc
    | head :: rest -> take_inner (n - 1) (head :: acc) rest
    | _ -> failwith "unreachable"
  in
  take_inner n [] list

let () =
  let elves = rev (sort Stdlib.compare (groups input)) in
  let part1 = nth elves 0 in
  let part2 = sum (take 3 elves) in
  Printf.printf "part1: %d\n" part1;
  Printf.printf "part2: %d\n" part2
