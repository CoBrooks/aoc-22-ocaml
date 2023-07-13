open Stdlib.List

let sum list = fold_left ( + ) 0 list

let take n list =
  let rec take_inner n acc rest =
    match rest with
    | _ when n == 0 -> rev acc
    | head :: rest -> take_inner (n - 1) (head :: acc) rest
    | _ -> failwith "unreachable"
  in
  take_inner n [] list

let bifurcate list =
  let rec half acc list =
    match list with
    | a when length a <= length acc -> rev acc
    | head :: rest -> half (head :: acc) rest
    | _ -> failwith "unreachable"
  in
  (half [] list, rev (half [] (rev list)))

let dedup a =
  let rec inner acc a =
    match a with
    | [] -> acc
    | head :: rest ->
        if for_all (fun c -> c != head) acc then inner (head :: acc) rest
        else inner acc rest
  in
  rev (inner [] a)

let union a b =
  let a = dedup a in
  let b = dedup b in
  let rec inner acc a b =
    match a with
    | [] -> acc
    | head :: rest ->
        let dups = filter (fun c -> c == head) b in
        inner (dups @ acc) rest b
  in
  rev (inner [] a b)

let unions l =
  let rec inner acc l =
    match l with [] -> acc | head :: rest -> inner (union acc head) rest
  in
  rev (inner (nth l 0) l)

let groups n list =
  let rec inner acc groups list =
    match list with
    | _ when length acc == n -> inner [] (rev acc :: groups) list
    | head :: rest -> inner (head :: acc) groups rest
    | [] -> if length acc > 0 then rev acc :: groups else groups
  in
  rev (inner [] [] list)
