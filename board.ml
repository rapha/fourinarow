type t = Board of Player.t option list list

(* enum util functions *)

let rev enum = enum |> List.of_enum |> List.rev |> List.enum

let nth n = Enum.skip n |- Enum.peek

let transpose len matrix =
  let cross_section n = matrix |> Enum.clone |> map (nth n |- Option.default None) in
  map cross_section (0 -- (len-1))

let rotate_left i vector = 
  Enum.append (Enum.skip i vector) (Enum.take i vector)

let rotate_right i = 
  rev |- rotate_left i |- rev

let trim predicate =
  Enum.drop_while predicate |- rev |- Enum.drop_while predicate |- rev

let contains sub_list full_list =
  let rec rest_contains sub rest =
    match (sub, rest) with 
      | ([], _) -> true
      | (_, []) -> false
      | (x::sub_tail, y::rest_tail) when x = y -> rest_contains sub_tail rest_tail
      | (_, y::rest_tail) -> rest_contains sub_list rest_tail
  in rest_contains sub_list full_list


(* board-specific helpers *)

let row_length, col_length = 7, 6

let columns (Board cols) = cols |> List.enum |> map List.enum
let rows = columns |- transpose col_length

let tilt_left = map (flip (Enum.append) (Enum.repeat ~times:(col_length-1) None)) |- Enum.mapi rotate_right
let tilt_right = map (Enum.append (Enum.repeat ~times:(col_length-1) None)) |- Enum.mapi rotate_left

let diagonals tilt board = 
  board |> rows |> rev |> tilt |> transpose (col_length + row_length) |> map (trim ((=) None))

let north_east board = diagonals tilt_left board
let north_west board = diagonals tilt_right board

(* API *)

let empty = Board (List.make row_length [])

let drop player col (Board cols) = 
  let column = List.nth cols (col-1) in
  if (List.length column >= col_length) then failwith "column full" else ();
  let new_column = column @ [Some player] and
      before = (List.take (col-1) cols) and
      after  = (List.drop col cols) 
  in Board (before @ [new_column] @ after)

let wins player board = 
  let four_in_a_row = List.make 4 (Some player) in
  List.enum [columns; rows; north_east; north_west]
  |> map ((|>) board) 
  |> exists (exists (List.of_enum |- contains four_in_a_row))

let top_row col (Board cols) =
  List.nth cols (col-1) |> List.filter Option.is_some |> List.length

let to_string = 
  let cell_to_string = function Some p -> Player.to_string p | None -> "-" in
  let row_to_string = map cell_to_string |- reduce (^) |- flip (^) "\n" in
  rows |- map row_to_string |- reduce (^)

let build rows =
  let str_to_player = function "A" -> Some Player.A | "B" -> Some Player.B | _ -> None
  in rows |> List.enum 
    |> map (Str.split (Str.regexp "") |- List.enum |- map str_to_player)
    |> transpose col_length
    |> map (filter (not -| (=) None) |- map Option.get)
    |> Enum.foldi
      (fun i players board -> players |> fold (fun b player -> b |> drop player (i+1)) board)
      empty
