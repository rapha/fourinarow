type t = Board of Piece.t option list list
exception Column_full of int

(* some handy functions on List *)

let rotate_left n xs =
  List.drop n xs @ List.take n xs

let rotate_right n =
  List.rev |- rotate_left n |- List.rev

let trim predicate =
  List.dropwhile predicate |- List.rev |- List.dropwhile predicate |- List.rev

let transpose len matrix =
  let cross_section i = matrix |> List.map (fun vector -> try List.nth vector i with List.Invalid_index _ -> None) in
  List.map cross_section (List.init len identity)

let contains sub_list full_list =
  let rec rest_contains sub rest = match (sub, rest) with
    | ([], _) -> true
    | (_, []) -> false
    | (x::sub_tail, y::rest_tail) when x = y -> rest_contains sub_tail rest_tail
    | (_, y::rest_tail) -> rest_contains sub_list rest_tail
  in rest_contains sub_list full_list

(* private *)

let row_length, col_length = 7, 6

let columns (Board cols) = cols
let rows = columns |- transpose col_length

let tilt_left, tilt_right =
  let padding = (List.make (col_length-1) None) in
  let tilter pad rotate = List.map pad |- List.mapi rotate in
  (tilter (flip (@) padding) rotate_right, tilter ((@) padding) rotate_left)

let diagonals tilt =
  rows |- List.rev |- tilt |- transpose (col_length + row_length) |- List.map (trim ((=) None))

let north_east, north_west = (diagonals tilt_left, diagonals tilt_right)

(* public *)

let empty = Board (List.make row_length [])

let drop player col board =
  let cols = columns board in
  let column = List.nth cols col in
  if (List.length column >= col_length) then raise (Column_full col) else
  let new_column = column @ [Some player] and
      before = (List.take col cols) and
      after  = (List.drop (col+1) cols)
  in Board (before @ [new_column] @ after)

let wins player board =
  let four_in_a_row = List.make 4 (Some player) in
  [columns; rows; north_east; north_west]
  |> List.map ((|>) board)
  |> List.exists (List.exists (contains four_in_a_row))

let top_row col board =
  List.nth (columns board) col |> List.filter Option.is_some |> List.length

let to_string =
  let cell_to_string = function Some p -> Piece.to_string p | None -> "-" in
  let row_to_string = List.map cell_to_string |- List.reduce (^) |- flip (^) "\n" in
  rows |- List.rev |- List.map row_to_string |- List.reduce (^)

let build rows =
  let cols = rows |> List.map (Str.split (Str.regexp "") |- List.map Piece.of_string) |> transpose row_length in
  Board cols

let evaluate _ _ = 0.
