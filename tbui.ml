let winner = ref None

let read_column _ =
  input_line stdin |> int_of_string

let on_win = function
  | Game.Win a -> winner := Some a
  | _ -> ()

let print game =
  game |> Game.to_string |> Printf.printf "%s\n"; flush stdout; game

let rec loop game =
  match !winner with
  | Some p -> p |> Player.to_string |> Printf.printf "%s wins\n"
  | None ->
      let play move = Game.play_turn move game in
      read_column |> play |> print |> loop

;;
Game.create Player.A Player.B |> Game.handle on_win |> loop
