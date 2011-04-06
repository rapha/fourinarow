open Batteries

exception Column_full

type 'a col = Column of ('a option * 'a option * 'a option * 'a option * 'a option * 'a option) 


type 'a t = {
  first : 'a col;
  second : 'a col;
  third : 'a col;
  fourth: 'a col;
  fifth: 'a col;
  sixth : 'a col;
  seventh : 'a col;
}

let column_indices = 
  let open Col in
  [Col1; Col2; Col3; Col4; Col5; Col6; Col7]

let row_indices =
  let open Row in
  [Row1;Row2;Row3;Row4;Row5;Row6]

let empty = {
  first = Column (None, None, None, None, None, None);
  second = Column (None, None, None, None, None, None);
  third = Column (None, None, None, None, None, None);
  fourth = Column (None, None, None, None, None, None);
  fifth = Column (None, None, None, None, None, None);
  sixth = Column (None, None, None, None, None, None);
  seventh = Column (None, None, None, None, None, None)
}

let get_col col_index cols = 
  let open Col in
  match col_index with
  | Col1 -> cols.first
  | Col2 -> cols.second
  | Col3 -> cols.third
  | Col4 -> cols.fourth
  | Col5 -> cols.fifth
  | Col6 -> cols.sixth
  | Col7 -> cols.seventh

let set_col col_index col cols =
  let open Col in
  match col_index with
  | Col1 -> { cols with first = col }
  | Col2 -> { cols with second = col }
  | Col3 -> { cols with third = col }
  | Col4 -> { cols with fourth = col }
  | Col5 -> { cols with fifth = col }
  | Col6 -> { cols with sixth = col }
  | Col7 -> { cols with seventh = col }

let get_cell cols row_index col_index =
  let (Column (r1,r2,r3,r4,r5,r6)) = get_col col_index cols in
  let open Row in
  match row_index with
  | Row1 -> r1
  | Row2 -> r2
  | Row3 -> r3
  | Row4 -> r4
  | Row5 -> r5
  | Row6 -> r6

let set_cell cols row_index col_index cell =
  let (Column (r1,r2,r3,r4,r5,r6)) = get_col col_index cols in
  let open Row in
  let col = 
    match row_index with
    | Row1 -> Column (cell, r2, r3, r4, r5, r6)
    | Row2 -> Column (r1, cell, r3, r4, r5, r6)
    | Row3 -> Column (r1, r2, cell, r4, r5, r6)
    | Row4 -> Column (r1, r2, r3, cell, r5, r6)
    | Row5 -> Column (r1, r2, r3, r4, cell, r6)
    | Row6 -> Column (r1, r2, r3, r4, r5, cell)
  in
  set_col col_index col cols

let append_to piece col = 
  let open Row in
  match col with
  | Column (None, r2, r3, r4, r5, r6) -> (Column (Some piece, r2, r3, r4, r5, r6), Row1)
  | Column (r1, None, r3, r4, r5, r6) -> (Column (r1, Some piece, r3, r4, r5, r6), Row2)
  | Column (r1, r2, None, r4, r5, r6) -> (Column (r1, r2, Some piece, r4, r5, r6), Row3)
  | Column (r1, r2, r3, None, r5, r6) -> (Column (r1, r2, r3, Some piece, r5, r6), Row4)
  | Column (r1, r2, r3, r4, None, r6) -> (Column (r1, r2, r3, r4, Some piece, r6), Row5)
  | Column (r1, r2, r3, r4, r5, None) -> (Column (r1, r2, r3, r4, r5, Some piece), Row6)
  | Column (Some _, Some _, Some _, Some _, Some _, Some _) -> raise Column_full

let append col_index piece cols =
  let (appended, row_index) = cols |> get_col col_index |> append_to piece in
  (cols |> set_col col_index appended, row_index)

let to_string piece_to_string cols =
  let cell_to_string = function
    | Some piece -> piece_to_string piece
    | None -> "-"
  in
  let row_to_string row_index = 
    column_indices |> List.map (get_cell cols row_index) |> List.map cell_to_string |> String.join "" 
  in
  row_indices |> List.rev |> List.map row_to_string |> String.join "\n" |> flip (^) "\n"

let of_string piece_of_string str =
  let cell_of_string = function
    | "-" -> None
    | s   -> Some (piece_of_string s)
  in
  let row_of_string (row_str:string) : ('a option list) =
    row_str 
    |> flip String.nsplit " "
    |> List.map cell_of_string
  in
  let indexed_rows = str |> flip String.nsplit "\n" |> List.combine (row_indices |> List.rev) in
  indexed_rows |> List.fold_left (fun cols (row_index, row_str) ->
    let indexed_cells = row_str |> row_of_string |> List.combine column_indices in
    indexed_cells |> List.fold_left (fun cols (col_index, cell) ->
      set_cell cols row_index col_index cell
    ) cols
  ) empty 
