open List
open Util

open Player

type board = Board of player option list list
let row_length, col_length = 7, 6

let empty_board = Board (list_of row_length [])

let columns = function Board cols -> cols
let rows = columns >> transpose col_length
  
let string_of_board = 
  let string_of_cell = function Some p -> string_of_player p | None -> "-" in
  let string_of_row = map string_of_cell >> fold_left (^) "" in
  rows >> rev >> map string_of_row >> fold_left (fun sofar line -> sofar ^ line ^ "\n") ""

let trim predicate =
  let rec trim_head = function
    | [] -> []
    | head::tail as list -> if predicate head then list else trim_head tail
  in
  trim_head >> rev >> trim_head >> rev

let tilt_left = map (fun row -> row @ list_of (col_length-1) None) >> mapi rotate_right
let tilt_right = map (fun row -> list_of (col_length-1) None @ row) >> mapi rotate_left

let diagonals tilt = 
  rows >> 
  rev >> 
  tilt >> 
  transpose (col_length + row_length) >> 
  map (trim is_some)

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

let has_four_in_a_row player =
  let four_in_a_row = list_of 4 (Some player)
  and contains_sublist sub_list full_list =
    let rec rest_contains_sublist sub rest =
      match (sub, rest) with 
        | ([], _) -> true
        | (_, []) -> false
        | (x::sub_tail, y::rest_tail) when x = y -> rest_contains_sublist sub_tail rest_tail
        | (_, y::rest_tail) -> rest_contains_sublist sub_list rest_tail
    in rest_contains_sublist sub_list full_list
  in exists (contains_sublist four_in_a_row)

let wins player board = 
  [columns; rows; north_east; north_west] 
    |> map ((|>) board)
    |> exists (has_four_in_a_row player)

let top col board =
  nth (columns board) (col-1) |> filter is_some |> length
