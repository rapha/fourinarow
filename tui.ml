open Batteries
open Printf
module TuiAi = Ai.Make(Board)
module TuiGame = Game.Make(Board)

let winner = ref None

let depth =
  try Sys.argv.(1) |> int_of_string
  with _ -> 4

let prompt_column _ _ =
  printf "column: "; flush stdout;
  input_line stdin |> int_of_string |> pred

let print game =
  game |> TuiGame.to_string |> printf "%s\n"; flush stdout;
  game

let rec loop game =
  match !winner with
  | Some p -> p |> Piece.to_string |> printf "%s has won\n"
  | None -> game |> TuiGame.play_turn |> print |> loop

let _ =
  let players = Player.create_pair (prompt_column, TuiAi.choose_column depth) in
  TuiGame.create players |> TuiGame.on_win (fun piece -> winner := Some piece) |> loop
