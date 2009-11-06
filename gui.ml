open Tk

open Game
open Player
open Util

let _ =
  let game = ref new_game in

  let colour = function A -> `Yellow | B -> `Red in
  let name = function A -> "Yellow" | B -> "Red" in

  let widget = openTk () in

  let button_row =
    let button col =
      let drop_command _ = 
        game := play_turn (fun _ -> col + 1) !game in
      Button.create ~text:"v" ~command:drop_command widget in
    List.map button (0 |-> 7) in

  let on_drop = function
    | Drop (row, col, player) -> 
        let piece = Label.create ~text:"    " ~background:(colour player) widget in
        grid ~row:(7-row) ~column:(col-1) [piece]
    | _ -> () in

  let on_switch = function
    | Switch player -> List.iter (fun button -> Button.configure button ~highlightbackground:(colour player)) button_row
    | _ -> () in

  let on_win = function
    | Win player -> 
        let message = (name player) ^ " wins" in 
        Dialog.create ~parent:widget ~title:"Game Over" ~message:message ~buttons:["OK"] ~default:0 () |> ignore;
        closeTk ()
    | _ -> () in

  game := !game |> handle on_drop |> handle on_win |> handle on_switch;

  grid ~row:0 button_row;
  foldi (fun _ row ->
    foldi (fun _ col -> 
      grid ~column:col ~row:(row+1) [Label.create ~text:"." ~borderwidth:4 widget]
    ) () 7
  ) () 6;

  Printexc.print mainLoop ()
