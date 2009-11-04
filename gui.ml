open Tk

open Game
open Player
open Util

let _ =
  let widget = openTk () in

  let on_drop = function
    | Drop (row, col, player) -> 
        let label = (string_of_player player) in
        grid ~row:(7-row) ~column:(col-1) [Button.create ~text:label widget]
    | _ -> () 

  and on_win = function
    | Win player -> 
        let message = (string_of_player player) ^ " wins" in 
        Dialog.create ~parent:widget ~title:"Game Over" ~message:message ~buttons:["OK"] () |> ignore;
        closeTk ()
    | _ -> () in

  let game = new_game |> handle on_drop |> handle on_win |> ref in

  let drop col _ = (game := play_turn (fun _ -> col) !game; ()) in

  foldi (fun _ row ->
    foldi (fun _ col -> 
      let button = 
        if row = 0 then Button.create ~text:"v" ~command:(drop (col+1)) widget else Button.create ~text:"-" widget 
      in grid ~column:col ~row:row [button]
    ) () 7
  ) () 7;

  Printexc.print mainLoop ()
