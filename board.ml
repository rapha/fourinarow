open List
open Util

open Player

type board = Board of player option list list
let row_length, col_length = 7, 6

let empty_board = Board (list_of row_length [])

let columns board = match board with Board cols -> cols
let rows board = columns board |> transpose
  
let string_of_board board = 
  let string_of_cell = function Some p -> string_of_player p | None -> "-" in
  let string_of_row row = map string_of_cell row |> fold_left (^) "" in
  rows board |> rev |> map string_of_row |> fold_left (fun sofar line -> sofar ^ line ^ "\n") ""

let tilt_left matrix = matrix |> map (fun row -> row @ list_of (col_length-1) None) |> mapi rotate_right
let tilt_right matrix = matrix |> map (fun row -> list_of (col_length-1) None @ row) |> mapi rotate_left

let diagonals tilt board = 
  rows board |> 
  rev |> 
  tilt |>
  transpose |> 
  map (filter is_some)

let north_east = diagonals tilt_left
let north_west = diagonals tilt_right

let drop player col board = 
  let cols = columns board in
  let column = nth cols (col-1) in
  if (length column >= col_length) then failwith "column full" else ();
  let new_column = column @ [Some player] and
      before = (sublist cols 0   (col-1)) and
      after  = (sublist cols col (length cols)) 
  in Board (before @ [new_column] @ after)

let has_four_in_a_row player lines =
  let four_in_a_row = list_of 4 (Some player)
  and contains_sublist sub_list full_list =
    let rec rest_contains_sublist sub rest =
      match (sub, rest) with 
        | ([], _) -> true
        | (_, []) -> false
        | (x::sub_tail, y::rest_tail) when x = y -> rest_contains_sublist sub_tail rest_tail
        | (_, y::rest_tail) -> rest_contains_sublist sub_list rest_tail
    in rest_contains_sublist sub_list full_list
  in lines |> exists (contains_sublist four_in_a_row)

let wins player board = 
  [columns; rows; north_east; north_west] 
    |> map ((|>) board)
    |> exists (has_four_in_a_row player)

