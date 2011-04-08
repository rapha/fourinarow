open Batteries

module GuiGame = Game.Make(Board)
open Tk

let _ =
  let chosen_col = ref Col.Col5 in
  let col_height = [|0; 0; 0; 0; 0; 0; 0|] in

  let use_chosen_col _ _ = !chosen_col in
  let game = ref (Player.create_pair (use_chosen_col,use_chosen_col) |> GuiGame.create) in

  let colour = function Piece.Yellow -> `Yellow | Piece.Red -> `Red in

  let widget = openTk () in

  let button_row =
    let button col =
      let drop_command _ = 
        chosen_col := Col.of_int col;
        game := GuiGame.play_turn !game;
        col_height.(col) <- col_height.(col) + 1 
      in
      Button.create ~text:"v" ~command:drop_command widget in
    map button (0 -- 6) |> List.of_enum in

  grid ~row:0 button_row;
  for row = 1 to 6 do
    for col = 0 to 6 do
      grid ~column:col ~row:row [Label.create ~text:"." ~borderwidth:4 widget]
    done
  done;

  let drop_handler (col, piece) =
    let piece = Label.create ~text:"    " ~background:(colour piece) widget in
    let row = col_height.(Col.to_int col) in
    grid ~row:(6-row) ~column:(Col.to_int col) [piece] in

  let switch_handler piece =
    List.iter (fun button -> Button.configure button ~highlightbackground:(colour piece)) button_row in

  let win_handler piece =
    let message = (piece |> Piece.to_string) ^ " has won" in 
    Dialog.create ~parent:widget ~title:"MyGame Over" ~message:message ~buttons:["OK"] ~default:0 () |> ignore;
    closeTk () in

  game := !game |> GuiGame.on_drop drop_handler |> GuiGame.on_win win_handler |> GuiGame.on_switch switch_handler;

  mainLoop ()
