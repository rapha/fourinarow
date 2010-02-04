open Game.Normal
module AI = Ai.Make(Board)

let winner = ref None

let read_column _ =
  input_line stdin |> int_of_string

let on_win = function
  | Win a -> winner := Some a
  | _ -> ()

let print game =
  game |> to_string |> Printf.printf "%s\n"; flush stdout; game

let rec loop game =
  match !winner with
  | Some p -> p |> Player.to_string |> Printf.printf "%s wins\n"
  | None ->
      let get_column =
        let player = game |> players |> fst in
        if player = Player.A then
          read_column
        else
          AI.choose_column 4
      in
      let play move = play_turn move game in
      get_column |> play |> print |> loop

;;
create Player.A Player.B |> handle on_win |> loop
