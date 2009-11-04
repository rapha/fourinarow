open Tk

open Game
open Player
open Util

let game = ref new_game

let on_drop container = function
  | Drop (row, col, player) ->
    let lable = string_of_player player in
    grid ~row:(7-row) ~column:(col-1) [Button.create ~text:lable container]
  | _ -> ()

let drop col _ = 
  game := play_turn (fun _ -> col) !game;
  match winner !game with 
    | Some player -> player |> string_of_player |> Printf.printf "%s wins\n"; closeTk ()
    | None -> ()

let setup () =
  let top = openTk () in
  foldi (fun _ row ->
    foldi (fun _ col -> 
      let button = 
        if row = 0 then Button.create ~text:"v" ~command:(drop (col+1)) top else Button.create ~text:"-" top 
      in grid ~column:col ~row:row [button]
    ) () 7
  ) () 7;
  game := handle (on_drop top) !game;
  ()

let _ = setup () ; Printexc.print mainLoop ()
