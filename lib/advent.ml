let read_input path =
  let file = Stdlib.open_in path in
  let rec read_lines acc =
    try read_lines (input_line file :: acc)
    with End_of_file ->
      close_in file;
      List.rev acc
  in
  read_lines []

let sum list = List.fold_left ( + ) 0 list
