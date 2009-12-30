open Util

type t = Board of Player.t option list list
let row_length, col_length = 7, 6

let columns = function Board cols -> cols
let rows = columns |- transpose col_length

let list_of count value = Enum.repeat ~times:count value |> List.of_enum
  
let trim predicate =
  let rec trim_head = function
    | [] -> []
    | head::tail as list -> if predicate head then list else trim_head tail
  in
  trim_head |- List.rev |- trim_head |- List.rev

let tilt_left = List.map (fun row -> row @ list_of (col_length-1) None) |- List.mapi rotate_right
let tilt_right = List.map (fun row -> list_of (col_length-1) None @ row) |- List.mapi rotate_left

let diagonals tilt = 
  rows |- List.rev |- tilt |- transpose (col_length + row_length) |- List.map (trim Option.is_some)

let north_east = diagonals tilt_left
let north_west = diagonals tilt_right

let has_four_in_a_row player line = 
  let four_in_a_row = list_of 4 (Some player) in
  let contains_sublist sub_list full_list =
    let rec rest_contains_sublist sub rest =
      match (sub, rest) with 
        | ([], _) -> true
        | (_, []) -> false
        | (x::sub_tail, y::rest_tail) when x = y -> rest_contains_sublist sub_tail rest_tail
        | (_, y::rest_tail) -> rest_contains_sublist sub_list rest_tail
    in rest_contains_sublist sub_list full_list
  in List.exists (contains_sublist four_in_a_row) line

(* API *)

let empty = Board (list_of row_length [])

let drop player col board = 
  let cols = columns board in
  let column = List.nth cols (col-1) in
  if (List.length column >= col_length) then failwith "column full" else ();
  let new_column = column @ [Some player] and
      before = (sublist 0   (col-1) cols) and
      after  = (sublist col (List.length cols) cols) 
  in Board (before @ [new_column] @ after)

let to_string = 
  let cell_to_string = function Some p -> Player.to_string p | None -> "-" in
  let row_to_string = List.map cell_to_string |- List.fold_left (^) "" in
  rows |- List.rev |- List.map row_to_string |- List.fold_left (fun sofar line -> sofar ^ line ^ "\n") ""

let wins player board = 
  [columns; rows; north_east; north_west] |> List.map ((|>) board) |> List.exists (has_four_in_a_row player)

let top_row col board =
  List.nth (columns board) (col-1) |> List.filter Option.is_some |> List.length
