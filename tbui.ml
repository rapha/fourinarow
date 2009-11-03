open Game
open Player
open Util

let prompt_for_column _ =
  input_line stdin |> int_of_string

let rec loop game =
  let game = play_turn prompt_for_column game in
  game |> string_of_game |> print_endline;
  match winner game with 
    | Some player -> player |> string_of_player |> Printf.printf "%s wins\n"
    | None -> loop game

;;
loop new_game
