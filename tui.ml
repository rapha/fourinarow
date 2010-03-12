open Game.Normal
open Printf
module AI = Ai.Make(Board.Make(Player))

let winner = ref None

let depth =
  try Sys.argv.(1) |> int_of_string
  with _ -> 4

let prompt_column _ =
  printf "column: "; flush stdout;
  input_line stdin |> int_of_string |> flip (-) 1

let on_win = function
  | Win a -> winner := Some a
  | _ -> ()

let print game =
  game |> to_string |> printf "%s\n"; flush stdout;
  game

let rec loop game =
  match !winner with
  | Some p -> p |> Player.to_string |> printf "%s wins\n"
  | None ->
      let choose_column =
        let player = game |> players |> fst in
        if player = Player.A then
          prompt_column
        else
          AI.choose_column depth
      in
      let play move = play_turn move game in
      choose_column |> play |> print |> loop

let _ =
  create Player.A Player.B |> handle on_win |> loop
