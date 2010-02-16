open Game.Normal
open Printf
module AI = Ai.Make(Board)

let winner = ref None

let prompt_column _ =
  printf "column: "; flush stdout;
  input_line stdin |> int_of_string

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
          AI.choose_column 4
      in
      let play move = play_turn move game in
      choose_column |> play |> print |> loop

let _ =
  create Player.A Player.B |> handle on_win |> loop
