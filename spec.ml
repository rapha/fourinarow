open Batteries

open Ospecl.Spec
open Ospecl.Matchers

module TestGame = Game.Make(Board)

let _ =
  let lines = Str.split (Str.regexp "\n") in
  let drop_in col _ _ = col in

  let (|>) x f = f x in

  Ospecl.Run.console [
    describe "Piece.to_char and Piece.of_char are inverse" [
        it "piece -> char -> piece" begin
          let equal_to_piece = equal_to Piece.to_string in

          Piece.Yellow |> Piece.to_char |> Piece.of_char =~ is (equal_to_piece Piece.Yellow)
        end;
        it "char -> piece -> char" begin
          'Y' |> Piece.of_char |> Piece.to_char =~ is (equal_to_char 'Y')
        end;
    ];
    describe "Board" begin 
      let open Col in 
      let equal_to_board = equal_to (fun _ -> "<board>") in
      [
        describe ".drop" [
          describe "in an Board.empty column" [
            it "puts a piece on the first row of that column" begin
              let expected = Board.of_string (
                "-------\n" ^
                "-------\n" ^
                "-------\n" ^
                "-------\n" ^
                "-------\n" ^
                "Y------\n" 
              ) 
              in
              Board.empty |> Board.drop Piece.Yellow Col1 =~ is (equal_to_board expected)
            end;
          ];
          describe "in a full column" [
            it "raises Column_full" begin
              let board = Board.of_string (
                "R------\n" ^
                "Y------\n" ^
                "R------\n" ^
                "Y------\n" ^
                "R------\n" ^
                "Y------\n" 
              ) 
              in
              (fun () -> board |> Board.drop Piece.Yellow Col1) =~ does (raise_exn (Board.Column_full Col1))
            end;
          ];
        ];

        describe ".has_won" [
          describe "on an Board.empty board" [
            it "is false for both players" begin
              ignore (Board.empty |> Board.has_won Piece.Yellow =~ is false');
              Board.empty |> Board.has_won Piece.Red =~ is false'
            end
          ];
          it "is false for vertical line of 3" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "Y------\n" ^
              "Y------\n" ^
              "Y------\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is false'
          end;
          it "is true for vertical line of 4" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "Y------\n" ^
              "Y------\n" ^
              "Y------\n" ^
              "Y------\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is true'
          end;
          it "is false for horizontal line of 3" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "YYY----\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is false'
          end;
          it "is true for horizontal line of 4" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "YYYY---\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is true'
          end;
          it "is false for NE line of 3" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "--Y----\n" ^
              "-YR----\n" ^
              "YRR----\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is false'
          end;
          it "is true for NE line of 4" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "---Y---\n" ^
              "--YR---\n" ^
              "-YRR---\n" ^
              "YRRR---\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is true'
          end;
          it "is false for NW line of 3" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "Y------\n" ^
              "RY-----\n" ^
              "RRY----\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is false'
          end;
          it "is true for NW line of 4" begin
            let board = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "Y------\n" ^
              "RY-----\n" ^
              "RRY----\n" ^
              "RRRY---\n")
            in 
            board |> Board.has_won Piece.Yellow =~ is true'
          end;
          it "is false when there is a gap in the line" begin
            let board = Board.of_string (
              "-------\n" ^
              "----R--\n" ^
              "---RR--\n" ^
              "--RYY--\n" ^
              "--YYY--\n" ^
              "R-YYY--\n")
            in 
            board |> Board.has_won Piece.Red =~ is false'
          end;
        ];
      ] 
    end;

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

        !dropped_in_col =~ is (equal_to_col_option (Some Col.Col3))
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

        !history =~ is (equal_to_piece_list [Piece.Red;Piece.Yellow;Piece.Red])
      end;
    ];

    describe "AI" [
      describe ".minimax" begin
        let equal_to_float = within 0.0001 in [
          
          it "returns 0 if Board.empty" begin
            let module TestAI = Ai.Make(Board) in

            Board.empty |> TestAI.minimax 0 Piece.Yellow Piece.Yellow TestAI.winning_score =~ is (equal_to_float 0.)
          end;
          it "returns losing score if opponent has won" begin
            let module TestAI = Ai.Make (struct include Board
              let has_won player board =
                player = Piece.Red
            end) in

            TestAI.minimax 0 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is (equal_to_float TestAI.losing_score)
          end;
          it "with depth 0 returns value from eval function" begin
            let module TestAI = Ai.Make (struct include Board
              let evaluate _ _ = 5.
            end) in

            TestAI.minimax 0 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is (equal_to_float 5.)
          end;
          it "with depth 1 returns winning score if player can win this turn" begin
            let module TestAI = Ai.Make (struct include Board
              let has_won player board =
                player = Piece.Yellow
            end) in

            TestAI.minimax 1 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is (equal_to_float TestAI.winning_score)
          end;
          it "with depth 1 returns highest values from eval function after this turn" begin
            let module TestAI = Ai.Make (struct include Board
              let last_drop = ref None 
              let drop _ col board = 
                last_drop := Some col;
                board
              let evaluate player board =
                match !last_drop with
                | Some Col.Col2 -> 5.
                | _             -> 0.
            end) in

            TestAI.minimax 1 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is (equal_to_float 5.)
          end;
          it "with depth 2 returns highest of lowest eval values after 2 turns" begin
            let module TestAI = Ai.Make (struct include Board
              let yellow_col = ref None
              let red_col = ref None

              let drop piece col board = 
                begin 
                  match piece with 
                  | Piece.Yellow -> yellow_col := Some (col |> Col.to_int |> succ)
                  | Piece.Red -> red_col := Some (col |> Col.to_int |> succ)
                end;
                board

              let evaluate player board =
                match (!yellow_col, !red_col) with
                | (Some y, Some r) -> y + r |> ( * ) (-1) |> float_of_int
                | _ -> failwith "should have dropped both a yellow and a red piece"
            end) in

            TestAI.minimax 2 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is (equal_to_float (-8.))
          end;
          it "with depth 1 returns an full score value for a full column" begin
            let module TestAI = Ai.Make (struct include Board
              let drop _ col _ =
                raise (Board.Column_full col)
            end) in

            TestAI.minimax 1 Piece.Yellow Piece.Yellow TestAI.column_full_score Board.empty =~ is (equal_to_float TestAI.column_full_score)
          end;
        ] 
        end;
        describe ".choose_column" [
          it "returns the move with the highest score" begin
            let equal_to_col = equal_to Col.to_string in

            let module Board = struct include Board
              let last_drop = ref None

              let drop _ col board =
                last_drop := Some col;
                board

              let evaluate _ board =
                match !last_drop with
                | Some Col.Col4 -> 1.
                | _ -> 0.
            end 
            in
            let module TestAI = Ai.Make (Board) in

            TestAI.choose_column 0 Board.empty Piece.Yellow =~ is (equal_to_col Col.Col4)
          end;
        ];
      ];
  ]
