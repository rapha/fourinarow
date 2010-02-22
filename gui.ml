open Game.Normal
open Tk

let _ =
  let game = ref (create Player.A Player.B) in

  let colour = function Player.A -> `Yellow | Player.B -> `Red in
  let name = function Player.A -> "Yellow" | Player.B -> "Red" in

  let widget = openTk () in

  let button_row =
    let button col =
      let drop_command _ = 
        game := play_turn (fun _ -> col) !game in
      Button.create ~text:"v" ~command:drop_command widget in
    map button (0 -- 6) |> List.of_enum in

  grid ~row:0 button_row;
  for row = 1 to 6 do
    for col = 0 to 6 do
      grid ~column:col ~row:row [Label.create ~text:"." ~borderwidth:4 widget]
    done
  done;

  let on_drop = function
    | Drop (row, col, player) -> 
        let piece = Label.create ~text:"    " ~background:(colour player) widget in
        grid ~row:(7-row) ~column:col [piece]
    | _ -> () in

  let on_switch = function
    | Switch player -> List.iter (fun button -> Button.configure button ~highlightbackground:(colour player)) button_row
    | _ -> () in

  let on_win = function
    | Win player -> 
        let message = (name player) ^ " wins" in 
        Dialog.create ~parent:widget ~title:"MyGame Over" ~message:message ~buttons:["OK"] ~default:0 () |> ignore;
        closeTk ()
    | _ -> () in

  game := !game |> handle on_drop |> handle on_win |> handle on_switch;

  Printexc.print mainLoop ()
