open Advent.Input
open Stdlib.List
open Stdlib.Seq

let input = read_input "inputs/day6.full" |> hd |> String.to_seq

let marker input p =
  let rec inner input n =
    let m =
      take p input |> of_seq |> sort Stdlib.compare |> to_seq |> group ( == )
    in
    match length m with
    | len when len == p -> n + p
    | _ -> inner (drop 1 input) (n + 1)
  in
  inner input 0

let () =
  let part1 = marker input 4 in
  let part2 = marker input 14 in
  Printf.printf "part 1: %i\n" part1;
  Printf.printf "part 2: %i\n" part2
