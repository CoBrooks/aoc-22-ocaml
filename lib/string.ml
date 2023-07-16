open Stdlib

let explode string =
  let rec exp string acc =
    if String.length string == List.length acc then acc
    else
      let ch = String.get string (List.length acc) in
      exp string (ch :: acc)
  in
  List.rev (exp string [])
