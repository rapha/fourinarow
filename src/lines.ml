open Batteries_uni

type cell = Row.t * Col.t

type t = { 
  yellow_lines : cell Line.t list;
  red_lines : cell Line.t list;
  free_lines : cell Line.t list;
} 

let all_winning_lines = 
  let open Row in 
  let open Col in 
  let line = Line.create in
  [
  (* vertical *)
  line (Row1,Col1) (Row2,Col1) (Row3,Col1) (Row4,Col1); line (Row2,Col1) (Row3,Col1) (Row4,Col1) (Row5,Col1); line (Row3,Col1) (Row4,Col1) (Row5,Col1) (Row6,Col1);
  line (Row1,Col2) (Row2,Col2) (Row3,Col2) (Row4,Col2); line (Row2,Col2) (Row3,Col2) (Row4,Col2) (Row5,Col2); line (Row3,Col2) (Row4,Col2) (Row5,Col2) (Row6,Col2);
  line (Row1,Col3) (Row2,Col3) (Row3,Col3) (Row4,Col3); line (Row2,Col3) (Row3,Col3) (Row4,Col3) (Row5,Col3); line (Row3,Col3) (Row4,Col3) (Row5,Col3) (Row6,Col3);
  line (Row1,Col4) (Row2,Col4) (Row3,Col4) (Row4,Col4); line (Row2,Col4) (Row3,Col4) (Row4,Col4) (Row5,Col4); line (Row3,Col4) (Row4,Col4) (Row5,Col4) (Row6,Col4);
  line (Row1,Col5) (Row2,Col5) (Row3,Col5) (Row4,Col5); line (Row2,Col5) (Row3,Col5) (Row4,Col5) (Row5,Col5); line (Row3,Col5) (Row4,Col5) (Row5,Col5) (Row6,Col5);
  line (Row1,Col6) (Row2,Col6) (Row3,Col6) (Row4,Col6); line (Row2,Col6) (Row3,Col6) (Row4,Col6) (Row5,Col6); line (Row3,Col6) (Row4,Col6) (Row5,Col6) (Row6,Col6);
  line (Row1,Col7) (Row2,Col7) (Row3,Col7) (Row4,Col7); line (Row2,Col7) (Row3,Col7) (Row4,Col7) (Row5,Col7); line (Row3,Col7) (Row4,Col7) (Row5,Col7) (Row6,Col7);
  (* horizontal *)
  line (Row1,Col1) (Row1,Col2) (Row1,Col3) (Row1,Col4); line (Row1,Col2) (Row1,Col3) (Row1,Col4) (Row1,Col5); line (Row1,Col3) (Row1,Col4) (Row1,Col5) (Row1,Col6); line (Row1,Col4) (Row1,Col5) (Row1,Col6) (Row1,Col7);
  line (Row2,Col1) (Row2,Col2) (Row2,Col3) (Row2,Col4); line (Row2,Col2) (Row2,Col3) (Row2,Col4) (Row2,Col5); line (Row2,Col3) (Row2,Col4) (Row2,Col5) (Row2,Col6); line (Row2,Col4) (Row2,Col5) (Row2,Col6) (Row2,Col7);
  line (Row3,Col1) (Row3,Col2) (Row3,Col3) (Row3,Col4); line (Row3,Col2) (Row3,Col3) (Row3,Col4) (Row3,Col5); line (Row3,Col3) (Row3,Col4) (Row3,Col5) (Row3,Col6); line (Row3,Col4) (Row3,Col5) (Row3,Col6) (Row3,Col7);
  line (Row4,Col1) (Row4,Col2) (Row4,Col3) (Row4,Col4); line (Row4,Col2) (Row4,Col3) (Row4,Col4) (Row4,Col5); line (Row4,Col3) (Row4,Col4) (Row4,Col5) (Row4,Col6); line (Row4,Col4) (Row4,Col5) (Row4,Col6) (Row4,Col7);
  line (Row5,Col1) (Row5,Col2) (Row5,Col3) (Row5,Col4); line (Row5,Col2) (Row5,Col3) (Row5,Col4) (Row5,Col5); line (Row5,Col3) (Row5,Col4) (Row5,Col5) (Row5,Col6); line (Row5,Col4) (Row5,Col5) (Row5,Col6) (Row5,Col7);
  line (Row6,Col1) (Row6,Col2) (Row6,Col3) (Row6,Col4); line (Row6,Col2) (Row6,Col3) (Row6,Col4) (Row6,Col5); line (Row6,Col3) (Row6,Col4) (Row6,Col5) (Row6,Col6); line (Row6,Col4) (Row6,Col5) (Row6,Col6) (Row6,Col7);
  (* north-east *)
  line (Row1,Col1) (Row2,Col2) (Row3,Col3) (Row4,Col4); line (Row2,Col1) (Row3,Col2) (Row4,Col3) (Row5,Col4); line (Row3,Col1) (Row4,Col2) (Row5,Col3) (Row6,Col4);
  line (Row1,Col2) (Row2,Col3) (Row3,Col4) (Row4,Col5); line (Row2,Col2) (Row3,Col3) (Row4,Col4) (Row5,Col5); line (Row3,Col2) (Row4,Col3) (Row5,Col4) (Row6,Col5);
  line (Row1,Col3) (Row2,Col4) (Row3,Col5) (Row4,Col6); line (Row2,Col3) (Row3,Col4) (Row4,Col5) (Row5,Col6); line (Row3,Col3) (Row4,Col4) (Row5,Col5) (Row6,Col6);
  line (Row1,Col4) (Row2,Col5) (Row3,Col6) (Row4,Col7); line (Row2,Col4) (Row3,Col5) (Row4,Col6) (Row5,Col7); line (Row3,Col4) (Row4,Col5) (Row5,Col6) (Row6,Col7);
  (* north-west *)
  line (Row1,Col7) (Row2,Col6) (Row3,Col5) (Row4,Col4); line (Row2,Col7) (Row3,Col6) (Row4,Col5) (Row5,Col4); line (Row3,Col7) (Row4,Col6) (Row5,Col5) (Row6,Col4); 
  line (Row1,Col6) (Row2,Col5) (Row3,Col4) (Row4,Col3); line (Row2,Col6) (Row3,Col5) (Row4,Col4) (Row5,Col3); line (Row3,Col6) (Row4,Col5) (Row5,Col4) (Row6,Col3); 
  line (Row1,Col5) (Row2,Col4) (Row3,Col3) (Row4,Col2); line (Row2,Col5) (Row3,Col4) (Row4,Col3) (Row5,Col2); line (Row3,Col5) (Row4,Col4) (Row5,Col3) (Row6,Col2); 
  line (Row1,Col4) (Row2,Col3) (Row3,Col2) (Row4,Col1); line (Row2,Col4) (Row3,Col3) (Row4,Col2) (Row5,Col1); line (Row3,Col4) (Row4,Col3) (Row5,Col2) (Row6,Col1); 
]

let empty = {
  yellow_lines = [];
  red_lines = [];
  free_lines = all_winning_lines;
}

let lines_for piece {yellow_lines; red_lines} =
  match piece with
  | Piece.Yellow -> yellow_lines
  | Piece.Red -> red_lines

let add piece cell ({yellow_lines; red_lines; free_lines} as lines) =
  let (new_lines, still_free) = free_lines |> List.partition (Line.includes cell) in
  let my_lines = lines |> lines_for piece |> List.append new_lines |> List.map (Line.fill cell) in
  let their_lines = lines |> lines_for (Piece.opponent piece) |> List.filter (not -| Line.includes cell) in
  match piece with
  | Piece.Yellow -> {yellow_lines = my_lines; red_lines = their_lines; free_lines = still_free}
  | Piece.Red -> {yellow_lines = their_lines; red_lines = my_lines; free_lines = still_free}

let has_won piece lines =
  lines |> lines_for piece |> List.exists Line.all_filled
