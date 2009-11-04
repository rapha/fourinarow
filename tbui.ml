open Game
open Player
open Util

let winner = ref None

let prompt_for_column _ =
  input_line stdin |> int_of_string

let on_win = function 
  | Win a -> winner := Some a
  | _ -> ()

let print game =
  game |> string_of_game |> Printf.printf "%s\n"; flush stdout;
  game

let rec loop game =
  match !winner with 
  | Some p -> p |> string_of_player |> Printf.printf "%s wins\n"
  | None -> game |> play_turn prompt_for_column |> print |> loop

;;
new_game |> handle on_win |> loop
