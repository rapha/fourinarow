open Tk

open Game
open Player
open Util

let _ =
  let game = ref new_game in
  let drop col _ = (game := play_turn (fun _ -> col) !game; ()) in

  let widget = openTk () in

  let colour player = match player with A -> `Yellow | B -> `Red in

  let piece player =
    Label.create ~text:"    " ~background:(colour player) widget in

  let add_button_row bgcolour =
    foldi (fun _ col -> grid ~column:col ~row:0 [Button.create ~text:"v" ~highlightbackground:bgcolour ~command:(drop (col+1)) widget]) () 7 in

  let on_drop = function
    | Drop (row, col, player) -> grid ~row:(7-row) ~column:(col-1) [piece player]
    | _ -> () in

  let on_switch = function
    | Switch player -> add_button_row (colour player)
    | _ -> () in

  let on_win = function
    | Win player -> 
        let message = (string_of_player player) ^ " wins" in 
        Dialog.create ~parent:widget ~title:"Game Over" ~message:message ~buttons:["OK"] ~default:0 () |> ignore;
        closeTk ()
    | _ -> () in

  game := !game |> handle on_drop |> handle on_win |> handle on_switch;

  add_button_row `Black;
  foldi (fun _ row ->
    foldi (fun _ col -> 
      grid ~column:col ~row:(row+1) [Label.create ~text:"." ~borderwidth:4 widget]
    ) () 7
  ) () 6;

  Printexc.print mainLoop ()
