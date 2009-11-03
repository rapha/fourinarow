open Tk

open Game
open Player
open Util

let game = ref new_game

let drop col _ = 
  game := play_turn (fun _ -> col) !game;
  !game |> string_of_game |> print_endline; flush stdout;
  match winner !game with 
    | Some player -> player |> string_of_player |> Printf.printf "%s wins\n"; closeTk ()
    | None -> ()

let top = openTk () 

;;
[1;2;3;4;5;6;7] |> List.map drop |> List.map (fun f -> Button.create ~text:"v" ~command:f top) |> grid
;
mainLoop ()
