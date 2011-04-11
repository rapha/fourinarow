open Batteries_uni

type t = Row1 | Row2 | Row3 | Row4 | Row5 | Row6

let bottom_to_top = [Row1; Row2; Row3; Row4; Row5; Row6]
let top_to_bottom = List.rev bottom_to_top

let to_int = function
  | Row1 -> 0
  | Row2 -> 1
  | Row3 -> 2
  | Row4 -> 3
  | Row5 -> 4
  | Row6 -> 5

let of_int = function
  | 0 -> Row1
  | 1 -> Row2
  | 2 -> Row3
  | 3 -> Row4
  | 4 -> Row5
  | 5 -> Row6
  | i -> invalid_arg (Printf.sprintf "Row index of %d is invalid. Must be in range [0-5]." i)

let to_string index = "Row" ^ (index |> to_int |> string_of_int)

