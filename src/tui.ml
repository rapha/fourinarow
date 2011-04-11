open Batteries_uni
open Printf
module TuiAi = Ai.Make(Board)
module TuiGame = Game.Make(Board)

let view = [|
  [|'-';'-';'-';'-';'-';'-';'-';|];
  [|'-';'-';'-';'-';'-';'-';'-';|];
  [|'-';'-';'-';'-';'-';'-';'-';|];
  [|'-';'-';'-';'-';'-';'-';'-';|];
  [|'-';'-';'-';'-';'-';'-';'-';|];
  [|'-';'-';'-';'-';'-';'-';'-';|];
|]
let row_heights = [|0;0;0;0;0;0;0|]

let depth =
  try Sys.argv.(1) |> int_of_string
  with _ -> 4

let prompt_column _ _ =
  printf "column: "; flush stdout;
  input_line stdin |> int_of_string |> flip (-) 1 |> Col.of_int

let print () =
  let join separator = Array.fold_left (flip (^) separator |- (^)) "" in

  view 
    |> Array.map (Array.map string_of_char |- join " ") 
    |> join "\n"
    |> print_endline;
  flush stdout

let rec loop game =
  game |> TuiGame.play_turn |> loop

let win_handler piece = 
  piece |> Piece.to_string |> printf "%s has won\n"; 
  exit 0

let drop_handler (col, piece) =
  let coli = col |> Col.to_int in
  let rowi = row_heights.(coli) in
  view.(5 - rowi).(coli) <- Piece.to_char piece;
  row_heights.(coli) <- rowi + 1;
  print ()

let _ =
  let players = Player.create_pair (prompt_column, TuiAi.choose_column depth) in
  TuiGame.create players 
    |> TuiGame.on_drop drop_handler 
    |> TuiGame.on_win win_handler
    |> loop
