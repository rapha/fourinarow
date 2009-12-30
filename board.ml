let transpose len matrix =
  let nth n = List.drop n |- List.enum |- Enum.peek |- Option.default None in
  let cross_section n = matrix |> List.map (nth n) in
  List.init len cross_section

let rotate_left i vector = 
  List.drop i vector @ List.take i vector

let rotate_right i = 
  List.rev |- rotate_left i |- List.rev

let trim predicate =
  let rec trim_head = function
    | [] -> []
    | head::tail as list -> if predicate head then list else trim_head tail
  in
  trim_head |- List.rev |- trim_head |- List.rev


type t = Board of Player.t option list list
let row_length, col_length = 7, 6

let columns = function Board cols -> cols
let rows = columns |- transpose col_length

let tilt_left = List.map (fun row -> row @ List.make (col_length-1) None) |- List.mapi rotate_right
let tilt_right = List.map (fun row -> List.make (col_length-1) None @ row) |- List.mapi rotate_left

let diagonals tilt = 
  rows |- List.rev |- tilt |- transpose (col_length + row_length) |- List.map (trim Option.is_some)

let north_east = diagonals tilt_left
let north_west = diagonals tilt_right

let has_four_in_a_row player line = 
  let four_in_a_row = List.make 4 (Some player) in
  let contains sub_list full_list =
    let rec rest_contains sub rest =
      match (sub, rest) with 
        | ([], _) -> true
        | (_, []) -> false
        | (x::sub_tail, y::rest_tail) when x = y -> rest_contains sub_tail rest_tail
        | (_, y::rest_tail) -> rest_contains sub_list rest_tail
    in rest_contains sub_list full_list
  in List.exists (contains four_in_a_row) line

(* API *)

let empty = Board (List.make row_length [])

let drop player col board = 
  let cols = columns board in
  let column = List.nth cols (col-1) in
  if (List.length column >= col_length) then failwith "column full" else ();
  let new_column = column @ [Some player] and
      before = (List.take (col-1) cols) and
      after  = (List.drop col cols) 
  in Board (before @ [new_column] @ after)

let wins player board = 
  [columns; rows; north_east; north_west] |> List.map ((|>) board) |> List.exists (has_four_in_a_row player)

let top_row col board =
  List.nth (columns board) (col-1) |> List.filter Option.is_some |> List.length

let to_string = 
  let cell_to_string = function Some p -> Player.to_string p | None -> "-" in
  let row_to_string = List.map cell_to_string |- List.fold_left (^) "" in
  rows |- List.rev |- List.map row_to_string |- List.fold_left (fun sofar line -> sofar ^ line ^ "\n") ""

let build rows =
  let str_to_player = function "A" -> Some Player.A | "B" -> Some Player.B | _ -> None
  in rows
    |> List.map (Str.split (Str.regexp "") |- List.map str_to_player)
    |> transpose row_length
    |> List.map (List.filter ((=) None |- not) |- List.map Option.get)
    |> List.fold_left2
      (fun board i players -> players |> List.fold_left (fun b player -> b |> drop player i) board)
      empty
      (1 -- row_length |> List.of_enum)
