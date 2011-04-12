#use "topfind"

#require "unix"
#require "ospecl"
open Ospecl.Spec
open Ospecl.Matchers

#require "batteries"
open Batteries_uni

#load "col.cmo"
#load "row.cmo"
#load "piece.cmo"
#load "line.cmo"
#load "lines.cmo"
#load "columns.cmo"
#load "board.cmo"
#load "player.cmo"
#load "game.cmo"

module TestGame = Game.Make(Board)

let specs =
  let drop_in col _ _ = col in

  [
    describe "Game.play_turn" [
      it "uses function passed in to get row to drop in" begin
        let equal_to_col_option = 
          let string_of_col_option = function 
            | None -> "None" 
            | Some n -> "Some " ^ (Col.to_string n) 
          in
          equal_to string_of_col_option
        in

        let dropped_in_col = ref None in
        let module TestGame = Game.Make (struct include Board
          let drop _ c board =
            dropped_in_col := Some c;
            board
        end) 
        in

        let players = Player.create_pair (drop_in Col.Col3, drop_in Col.Col1) in

        TestGame.create players |> TestGame.play_turn |> ignore;

        !dropped_in_col =~ is equal_to_col_option (Some Col.Col3)
      end;
      it "calls drop handler" begin
        let drop_handled = ref false in
        let handler = function (Col.Col4, Piece.Yellow) -> drop_handled := true | _ -> () in

        Player.create_pair (drop_in Col.Col4, drop_in Col.Col3) |> TestGame.create |> TestGame.on_drop handler |> 
        TestGame.play_turn |> ignore;

        !drop_handled =~ is true'
      end;
      it "calls switch player handler" begin
        let switch_handled = ref false in
        let handler = function Piece.Red -> switch_handled := true | _ -> () in

        Player.create_pair (drop_in Col.Col2, drop_in Col.Col3) |> TestGame.create |> TestGame.on_switch handler |> 
        TestGame.play_turn |> ignore;

        !switch_handled =~ is true'
      end;
      it "calls win handler" begin
        let win_handled = ref false in
        let handler = function Piece.Yellow -> win_handled := true | _ -> () in
        let play_turn = TestGame.play_turn in
        let game = Player.create_pair (drop_in Col.Col2, drop_in Col.Col3) |> TestGame.create |> TestGame.on_win handler |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn in

        game |> play_turn |> ignore;

        !win_handled =~ is true'
      end;
      it "toggles current piece" begin
        let history = ref [] in
        let handler a = history := (a :: !history) in
        Player.create_pair (drop_in Col.Col4, drop_in Col.Col3) |> TestGame.create |> TestGame.on_switch handler |>

        TestGame.play_turn |> TestGame.play_turn |> TestGame.play_turn |> ignore;

        let equal_to_piece_list = equal_to (List.fold_left (fun str piece -> str ^ "," ^ (Piece.to_string piece)) "") in

        !history =~ is equal_to_piece_list [Piece.Red;Piece.Yellow;Piece.Red]
      end;
    ];

  ]
