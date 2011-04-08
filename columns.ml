open Batteries

exception Full

let next_row row = 
  let open Row in
  match row with
  | None -> Row1
  | Some Row1 -> Row2
  | Some Row2 -> Row3
  | Some Row3 -> Row4
  | Some Row4 -> Row5
  | Some Row5 -> Row6
  | Some Row6 -> raise Full

type 'a t = {
  col1 : Row.t option;
  col2 : Row.t option;
  col3 : Row.t option;
  col4 : Row.t option;
  col5 : Row.t option;
  col6 : Row.t option;
  col7 : Row.t option;
}

let empty = {
  col1 = None;
  col2 = None;
  col3 = None;
  col4 = None;
  col5 = None;
  col6 = None;
  col7 = None;
}

let append col piece cols =
  let open Col in
  match col with 
  | Col1 -> ( { cols with col1 = Some (next_row cols.col1) }, (next_row cols.col1) )
  | Col2 -> ( { cols with col2 = Some (next_row cols.col2) }, (next_row cols.col2) )
  | Col3 -> ( { cols with col3 = Some (next_row cols.col3) }, (next_row cols.col3) )
  | Col4 -> ( { cols with col4 = Some (next_row cols.col4) }, (next_row cols.col4) )
  | Col5 -> ( { cols with col5 = Some (next_row cols.col5) }, (next_row cols.col5) )
  | Col6 -> ( { cols with col6 = Some (next_row cols.col6) }, (next_row cols.col6) )
  | Col7 -> ( { cols with col7 = Some (next_row cols.col7) }, (next_row cols.col7) )
(*
let to_string piece_to_string cols =
  let cell_to_string = function
    | Some piece -> piece_to_string piece
    | None -> "-"
  in
  let row_to_string row =
    Col.left_to_right |> List.map (get_cell cols row) |> List.map cell_to_string |> String.join ""
  in
  Row.top_to_bottom |> List.map row_to_string |> String.join "\n" |> flip (^) "\n"

let of_string piece_of_string str =
  let cell_of_string = function
    | "-" -> None
    | s   -> Some (piece_of_string s)
  in
  let row_of_string row_str =
    row_str
    |> flip String.nsplit " "
    |> List.map cell_of_string
  in
  let row_strings = str |> flip String.nsplit "\n" |> List.rev in
  row_strings |> List.fold_left (fun cols row_str ->
    let indexed_cells = row_str |> row_of_string |> List.combine Col.left_to_right in
    indexed_cells |> List.fold_left (fun cols (col, cell) ->
      match cell with
      | None -> cols
      | Some piece -> cols |> append col piece
    ) cols
  ) empty
*)
