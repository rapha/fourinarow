open Batteries

open Ospecl.Spec
open Ospecl.Matchers

module TestGame = Game.Make(Board)

let _ =
  let lines = Str.split (Str.regexp "\n") in
  let drop_in col _ _ = col in

  let (|>) x f = f x in

  Ospecl.Run.console [
    describe "Piece.to_string and Piece.of_string are inverse" [
        it "piece -> string -> piece" begin
          let equal_to_piece = equal_to Piece.to_string in

          Piece.A |> Piece.to_string |> Piece.of_string =~ is (equal_to_piece Piece.A)
        end;
        it "string -> piece -> string" begin
          "A" |> Piece.of_string |> Piece.to_string =~ is (equal_to_string "A")
        end;
    ];
    describe "Board" begin 
      let open Col_index in 
      let equal_to_board = equal_to Board.to_string in
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
              "A------\n" 
            ) 
            in
            Board.empty |> Board.drop Piece.A Col1 |> fst =~ is (equal_to_board expected)
          end;
        ];
        describe "in a full column" [
          it "raises Column_full" begin
            let board = Board.of_string (
              "B------\n" ^
              "A------\n" ^
              "B------\n" ^
              "A------\n" ^
              "B------\n" ^
              "A------\n" 
            ) 
            in
            (fun () -> board |> Board.drop Piece.A Col1) =~ does (raise_exn (Board.Column_full Col1))
          end;
        ];
      ];

      describe ".has_won" [
        describe "on an Board.empty board" [
          it "is false for both players" begin
            ignore (Board.empty |> Board.has_won Piece.A =~ is false');
            Board.empty |> Board.has_won Piece.B =~ is false'
          end
        ];
        it "is false for vertical line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "A------\n" ^
            "A------\n" ^
            "A------\n")
          in 
          board |> Board.has_won Piece.A =~ is false'
        end;
        it "is true for vertical line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "A------\n" ^
            "A------\n" ^
            "A------\n" ^
            "A------\n")
          in 
          board |> Board.has_won Piece.A =~ is true'
        end;
        it "is false for horizontal line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "AAA----\n")
          in 
          board |> Board.has_won Piece.A =~ is false'
        end;
        it "is true for horizontal line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "AAAA---\n")
          in 
          board |> Board.has_won Piece.A =~ is true'
        end;
        it "is false for NE line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "--A----\n" ^
            "-AB----\n" ^
            "ABB----\n")
          in 
          board |> Board.has_won Piece.A =~ is false'
        end;
        it "is true for NE line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "---A---\n" ^
            "--AB---\n" ^
            "-ABB---\n" ^
            "ABBB---\n")
          in 
          board |> Board.has_won Piece.A =~ is true'
        end;
        it "is false for NW line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "A------\n" ^
            "BA-----\n" ^
            "BBA----\n")
          in 
          board |> Board.has_won Piece.A =~ is false'
        end;
        it "is true for NW line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "A------\n" ^
            "BA-----\n" ^
            "BBA----\n" ^
            "BBBA---\n")
          in 
          board |> Board.has_won Piece.A =~ is true'
        end;
        it "is false when there is a gap in the line" begin
          let board = Board.of_string (
            "-------\n" ^
            "----B--\n" ^
            "---BB--\n" ^
            "--BAA--\n" ^
            "--AAA--\n" ^
            "B-AAA--\n")
          in 
          board |> Board.has_won Piece.B =~ is false'
        end;
      ];
      describe ".to_string" [
        it "for empty board is 6 row by 7 cols of -" begin
          let expected = (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" )
          in 
          Board.empty |> Board.to_string =~ is (equal_to_string expected)
        end;
        it "for with a single piece dropped in leftmost column shows piece at
        bottom left" begin
          let expected = (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "A------\n" )
          in 
          Board.empty |> Board.drop Piece.A Col1 |> fst |> Board.to_string =~ is (equal_to_string expected)
        end;
      ];
    ] end;

    describe "Game.play_turn" [
      it "uses function passed in to get row to drop in" begin
        let equal_to_col_option = 
          let string_of_col_option = function 
            | None -> "None" 
            | Some n -> "Some " ^ (Col_index.to_string n) 
          in
          equal_to string_of_col_option
        in

        let any_row = Row_index.Row1 in
        let dropped_in_col = ref None in
        let module TestGame = Game.Make (struct include Board
          let drop _ c board =
            dropped_in_col := Some c;
            (board, any_row)
        end) 
        in

        let players = Player.create_pair (drop_in Col_index.Col3, drop_in Col_index.Col1) in

        TestGame.create players |> TestGame.play_turn |> ignore;

        !dropped_in_col =~ is (equal_to_col_option (Some Col_index.Col3))
      end;
      it "calls drop handler" begin
        let drop_handled = ref false in
        let handler = function (Row_index.Row1, Col_index.Col4,Piece.A) -> drop_handled := true | _ -> () in

        Player.create_pair (drop_in Col_index.Col4, drop_in Col_index.Col3) |> TestGame.create |> TestGame.on_drop handler |> 
        TestGame.play_turn |> ignore;

        !drop_handled =~ is true'
      end;
      it "calls switch player handler" begin
        let switch_handled = ref false in
        let handler = function Piece.B -> switch_handled := true | _ -> () in

        Player.create_pair (drop_in Col_index.Col2, drop_in Col_index.Col3) |> TestGame.create |> TestGame.on_switch handler |> 
        TestGame.play_turn |> ignore;

        !switch_handled =~ is true'
      end;
      it "calls win handler" begin
        let win_handled = ref false in
        let handler = function Piece.A -> win_handled := true | _ -> () in
        let play_turn = TestGame.play_turn in
        let game = Player.create_pair (drop_in Col_index.Col2, drop_in Col_index.Col3) |> TestGame.create |> TestGame.on_win handler |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn in

        game |> play_turn |> ignore;

        !win_handled =~ is true'
      end;
      it "toggles current piece" begin
        let history = ref [] in
        let handler a = history := (a :: !history) in
        Player.create_pair (drop_in Col_index.Col4, drop_in Col_index.Col3) |> TestGame.create |> TestGame.on_switch handler |>

        TestGame.play_turn |> TestGame.play_turn |> TestGame.play_turn |> ignore;

        let equal_to_piece_list = equal_to (List.fold_left (fun str piece -> str ^ "," ^ (Piece.to_string piece)) "") in

        !history =~ is (equal_to_piece_list [Piece.B;Piece.A;Piece.B])
      end;
    ];

    describe "AI" [
      describe ".minimax" begin
        let equal_to_float = within 0.0001 in [
          
          it "returns 0 if Board.empty" begin
            let module TestAI = Ai.Make(Board) in

            Board.empty |> TestAI.minimax 0 Piece.A Piece.A TestAI.winning_score =~ is (equal_to_float 0.)
          end;
          it "returns losing score if opponent has won" begin
            let module TestAI = Ai.Make (struct include Board
              let has_won player board =
                player = Piece.B
            end) in

            TestAI.minimax 0 Piece.A Piece.A TestAI.winning_score Board.empty =~ is (equal_to_float TestAI.losing_score)
          end;
          it "with depth 0 returns value from eval function" begin
            let module TestAI = Ai.Make (struct include Board
              let evaluate _ _ = 5.
            end) in

            TestAI.minimax 0 Piece.A Piece.A TestAI.winning_score Board.empty =~ is (equal_to_float 5.)
          end;
          it "with depth 1 returns winning score if player can win this turn" begin
            let module TestAI = Ai.Make (struct include Board
              let has_won player board =
                player = Piece.A
            end) in

            TestAI.minimax 1 Piece.A Piece.A TestAI.winning_score Board.empty =~ is (equal_to_float TestAI.winning_score)
          end;
          it "with depth 1 returns highest values from eval function after this turn" begin
            let module TestAI = Ai.Make (struct include Board
              let evaluate player board =
                match board |> to_string |> lines |> List.last with
                | "-A-----" ->  5.
                | _         ->  0.
            end) in

            TestAI.minimax 1 Piece.A Piece.A TestAI.winning_score Board.empty =~ is (equal_to_float 5.)
          end;
          it "with depth 2 returns highest of lowest eval values after 2 turns" begin
            let module TestAI = Ai.Make (struct include Board
              let evaluate player board =
                let bottom_row = board |> to_string |> lines |> List.last in
                let col_with piece = 
                  try 
                    String.find bottom_row (piece |> Piece.to_string) |> (+) 1
                  with Not_found -> 
                    0
                in
                (col_with Piece.A) + (col_with Piece.B) |> ( * ) (-1) |> float_of_int
            end) in

            TestAI.minimax 2 Piece.A Piece.A TestAI.winning_score Board.empty =~ is (equal_to_float (-8.))
          end;
          it "with depth 1 returns an full score value for a full column" begin
            let module TestAI = Ai.Make (struct include Board
              let drop _ col _ =
                raise (Board.Column_full col)
            end) in

            TestAI.minimax 1 Piece.A Piece.A TestAI.column_full_score Board.empty =~ is (equal_to_float TestAI.column_full_score)
          end;
        ] 
        end;
        describe ".choose_column" [
          it "returns the move with the highest score" begin
            let equal_to_col = equal_to Col_index.to_string in

            let module Board = struct include Board
              let evaluate _ board =
                match board |> to_string |> lines |> List.last with
                | "---A---" -> 1.
                | _         -> 0.
            end in
            let module TestAI = Ai.Make (Board) in

            TestAI.choose_column 0 Board.empty Piece.A =~ is (equal_to_col Col_index.Col4)
          end;
        ];
      ];
  ]
