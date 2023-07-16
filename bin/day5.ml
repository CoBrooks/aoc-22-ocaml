open Advent.Input
open Advent.String
open Stdlib.Seq
open Stdlib.List

let input = read_input "inputs/day5.full"

let parse_crates input =
  let crates input =
    input |> to_seq
    |> take_while (fun s -> String.equal "" s |> not)
    |> of_seq |> map explode
  in
  let rotate ll =
    init (length (nth ll 0)) (fun i -> map (fun l -> nth l i) ll)
  in
  let columns crates =
    crates |> rotate |> map rev
    |> filter (fun l -> nth l 0 != ' ')
    |> map (filter (( != ) ' '))
  in
  let map columns = columns |> map (fun c -> (int_of_char (hd c) - 48, tl c)) in
  input |> crates |> columns |> map

type step = { n : int; src : int; dest : int }

let parse_proc input =
  let proc input =
    input |> rev |> to_seq
    |> take_while (fun s -> String.equal "" s |> not)
    |> of_seq |> rev
  in
  let parse_step line =
    line |> String.split_on_char ' '
    |> filteri (fun i _ -> i mod 2 == 1)
    |> map int_of_string
    |> function
    | [ n; src; dest ] -> { n; src; dest }
    | _ -> failwith "unreachable"
  in
  input |> proc |> map parse_step

let simulate_part1 crates proc =
  let step crates { n; src; dest } =
    let from = assoc src crates in
    let moved = from |> rev |> to_seq |> take n |> of_seq in
    let new_dest = assoc dest crates @ moved in
    let new_src = from |> to_seq |> take (length from - n) |> of_seq in
    crates |> remove_assoc src |> remove_assoc dest
    |> cons (dest, new_dest)
    |> cons (src, new_src)
  in
  let rec inner crates proc =
    match proc with
    | next :: rest -> inner (step crates next) rest
    | [] -> crates
  in
  inner crates proc

let simulate_part2 crates proc =
  let step crates { n; src; dest } =
    let from = assoc src crates in
    let moved = from |> rev |> to_seq |> take n |> of_seq |> rev in
    let new_dest = assoc dest crates @ moved in
    let new_src = from |> to_seq |> take (length from - n) |> of_seq in
    crates |> remove_assoc src |> remove_assoc dest
    |> cons (dest, new_dest)
    |> cons (src, new_src)
  in
  let rec inner crates proc =
    match proc with
    | next :: rest -> inner (step crates next) rest
    | [] -> crates
  in
  inner crates proc

let () =
  let crates sim =
    let crates = parse_crates input in
    let proc = parse_proc input in
    sim crates proc
  in
  let labels crates = 
    let labels, _ = split crates in
    labels |> sort Stdlib.compare
    |> map (fun i -> assoc i crates)
    |> map (fun s -> s |> rev |> hd)
    |> fun chars -> String.init (length chars) (nth chars)
  in
  let part1 = simulate_part1 |> crates |> labels in
  let part2 = simulate_part2 |> crates |> labels in
  Printf.printf "part 1: %s\n" part1;
  Printf.printf "part 2: %s\n" part2
