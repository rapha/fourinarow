open Tk

open Util

let _ =
  let game = ref (Game.create Player.A Player.B) in

  let colour = function Player.A -> `Yellow | Player.B -> `Red in
  let name = function Player.A -> "Yellow" | Player.B -> "Red" in

  let widget = openTk () in

  let button_row =
    let button col =
      let drop_command _ = 
        game := Game.play_turn (fun _ -> col + 1) !game in
      Button.create ~text:"v" ~command:drop_command widget in
    map button (0 -- 6) |> List.of_enum in

  let on_drop = function
    | Game.Drop (row, col, player) -> 
        let piece = Label.create ~text:"    " ~background:(colour player) widget in
        grid ~row:(7-row) ~column:(col-1) [piece]
    | _ -> () in

  let on_switch = function
    | Game.Switch player -> List.iter (fun button -> Button.configure button ~highlightbackground:(colour player)) button_row
    | _ -> () in

  let on_win = function
    | Game.Win player -> 
        let message = (name player) ^ " wins" in 
        Dialog.create ~parent:widget ~title:"Game Over" ~message:message ~buttons:["OK"] ~default:0 () |> ignore;
        closeTk ()
    | _ -> () in

  game := !game |> Game.handle on_drop |> Game.handle on_win |> Game.handle on_switch;

  grid ~row:0 button_row;
  for row = 1 to 6 do
    for col = 0 to 6 do
      grid ~column:col ~row:row [Label.create ~text:"." ~borderwidth:4 widget]
    done
  done
  ;

  Printexc.print mainLoop ()
