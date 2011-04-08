open Batteries
open Printf
module TuiAi = Ai.Make(Board)
module TuiGame = Game.Make(Board)

let winner = ref None
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

let print game =
  let str = Array.fold_left (fun sofar cells ->
    sofar ^ (Array.fold_left (fun s c -> s ^ " " ^ string_of_char c) "" cells) ^ "\n"
  ) "" view 
  in
  printf "%s\n" str; 
  flush stdout;
  game

let rec loop game =
  match !winner with
  | Some p -> p |> Piece.to_string |> printf "%s has won\n"
  | None -> game |> TuiGame.play_turn |> print |> loop

let win_handler piece = 
  winner := Some piece

let drop_handler (col, piece) =
  let coli = col |> Col.to_int in
  let rowi = row_heights.(coli) in
  view.(5 - rowi).(coli) <- Piece.to_char piece;
  row_heights.(coli) <- rowi + 1

let _ =
  let players = Player.create_pair (prompt_column, TuiAi.choose_column depth) in
  TuiGame.create players |> TuiGame.on_drop drop_handler |> TuiGame.on_win (fun piece -> winner := Some piece) |> loop
