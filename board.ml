open Batteries

type t = Board of Lines.t * Piece.t Columns.t

exception Column_full of Col.t

let empty = Board (Lines.empty, Columns.empty)

let columns (Board (_, cols)) = cols
let lines (Board (lines, _)) = lines

let drop piece col board =
  try 
    let (cols, row) = board |> columns |> Columns.append col piece in
    let lins = board |> lines |> Lines.add piece (row, col) in
    Board (lins, cols)
  with Columns.Full ->
    raise (Column_full col)

let has_won piece =
  lines |- Lines.has_won piece

let of_string str =
  let cell_of_char = function
    | '-' -> None
    | ch -> Some (Piece.of_char ch)
  in
  let cells = str 
    |> flip String.nsplit "\n"
    |> List.rev |> List.tl
    |> List.map (String.to_list |- List.map cell_of_char |- Array.of_list)
    |> Array.of_list
  in
  Col.left_to_right |> List.map Col.to_int |>
    List.fold_left (fun board col ->
      Row.bottom_to_top |> List.map Row.to_int |> List.fold_left (fun board row ->
        match cells.(row).(col) with 
        | None -> board
        | Some piece -> board |> drop piece (Col.of_int col)
      ) board
    ) empty 

let evaluate _ _ = 0.
