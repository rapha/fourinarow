open Batteries


type t = Board of Lines.t * Piece.t Columns.t

exception Column_full of Col_index.t

let empty = Board (Lines.empty, Columns.empty)

let columns (Board (_, cols)) = cols
let lines (Board (lines, _)) = lines

let drop piece col board =
  try
    let (cols, row) = board |> columns |> Columns.append col piece in
    let lins = board |> lines |> Lines.add piece (row, col) in
    (Board (lins, cols), row)
  with Columns.Column_full ->
    raise (Column_full col)

let has_won piece =
  lines |- Lines.has_won piece

let to_string =
  columns |- Columns.to_string Piece.to_string

let of_string str =
  let cell_of_char = function
    | '-' -> None
    | ch -> Some (Piece.of_string (String.of_char ch))
  in
  let cells = str 
    |> flip String.nsplit "\n"
    |> List.rev |> List.tl
    |> List.map (String.to_list |- List.map cell_of_char |- Array.of_list)
    |> Array.of_list
  in
  let module Rows = Row_index in
  let module Cols = Col_index in
  Cols.left_to_right |> List.map Cols.to_int |>
    List.fold_left (fun board col ->
      Rows.bottom_to_top |> List.map Rows.to_int |> List.fold_left (fun board row ->
        match cells.(row).(col) with 
        | None -> board
        | Some piece -> board |> drop piece (Cols.of_int col) |> fst
      ) board
    ) empty 

let evaluate _ _ = 0.
