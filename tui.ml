open Printf
module TuiAi = Ai.Make(Board)
module TuiGame = Game.Make(Board)

let winner = ref None

let depth =
  try Sys.argv.(1) |> int_of_string
  with _ -> 4

let prompt_column _ _ =
  printf "column: "; flush stdout;
  input_line stdin |> int_of_string |> flip (-) 1

let on_win = function
  | TuiGame.Win a -> winner := Some a
  | _ -> ()

let print game =
  game |> TuiGame.to_string |> printf "%s\n"; flush stdout;
  game

let rec loop game =
  match !winner with
  | Some p -> p |> Piece.to_string |> printf "%s wins\n"
  | None -> game |> TuiGame.play_turn |> print |> loop

let _ =
  let players = Player.create_pair prompt_column (TuiAi.choose_column depth) in
  TuiGame.create players |> TuiGame.handle on_win |> loop
