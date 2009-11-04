open Tk

open Game
open Player
open Util

let _ =
  let widget = openTk () in

  let on_drop = function
    | Drop (row, col, player) -> 
        let colour = match player with A -> `Yellow | B -> `Red in
        grid ~row:(7-row) ~column:(col-1) [Label.create ~text:"    " ~background:colour widget]
    | _ -> () 

  and on_win = function
    | Win player -> 
        let message = (string_of_player player) ^ " wins" in 
        Dialog.create ~parent:widget ~title:"Game Over" ~message:message ~buttons:["OK"] () |> ignore;
        closeTk ()
    | _ -> () in

  let game = new_game |> handle on_drop |> handle on_win |> ref in

  let drop col _ = (game := play_turn (fun _ -> col) !game; ()) in

  foldi (fun _ col -> grid ~column:col ~row:0 [Button.create ~text:"v" ~command:(drop (col+1)) widget]) () 7;
  foldi (fun _ row ->
    foldi (fun _ col -> 
      grid ~column:col ~row:(row+1) [Label.create ~text:"." ~borderwidth:4 widget]
    ) () 7
  ) () 6;

  Printexc.print mainLoop ()
