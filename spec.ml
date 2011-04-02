open Batteries

open Ospecl.Spec
open Ospecl.Matchers

module TestGame = Game.Make(Board)

let _ =
  let lines = Str.split (Str.regexp "\n") in
  let drop_in col _ _ = col in

  let (|>) x f = f x in

  Ospecl.Run.console [
    describe "Piece.to_string and Piece.of_string are inverse" begin
      let open Piece in [
        it "piece -> string -> piece" begin
          let equal_to_piece = equal_to to_string in

          A |> to_string |> of_string =~ is (equal_to_piece A)
        end;
        it "string -> piece -> string" begin
          "A" |> of_string |> to_string =~ is (equal_to_string "A")
        end;
    ] end;
    describe "Board" begin 
      let open Board in [
      describe ".drop" [
        describe "in an empty column" [
          it "puts a piece on the first row of that column" begin
            let expected = build ["A------"] |> to_string in

            empty |> drop Piece.A 0 |> to_string =~ is (equal_to_string expected)
          end;
        ];
        describe "in a full column" [
          it "raises Column_full" begin
            let board = build [
              "B------";
              "A------";
              "B------";
              "A------";
              "B------";
              "A------";
            ] in

            (fun () -> board |> drop Piece.A 0) =~ does (raise_exn (Column_full 0))
          end;
        ];
      ];

      describe ".has_won" [
        describe "on an empty board" [
          it "is false for both players" begin
            empty |> has_won Piece.A =~ is false';
            empty |> has_won Piece.B =~ is false'
          end
        ];
        it "is false for vertical line of 3" begin
          let board = build [
            "A------";
            "A------";
            "A------"]
          in board |> has_won Piece.A =~ is false'
        end;
        it "is true for vertical line of 4" begin
          let board = build [
            "A------";
            "A------";
            "A------";
            "A------"]
          in board |> has_won Piece.A =~ is true'
        end;
        it "is false for horizontal line of 3" begin
          let board = build [
            "AAA----"]
          in board |> has_won Piece.A =~ is false'
        end;
        it "is true for horizontal line of 4" begin
          let board = build [
            "AAAA---"]
          in board |> has_won Piece.A =~ is true'
        end;
        it "is false for NE line of 3" begin
          let board = build [
            "--A----";
            "-AB----";
            "ABB----"]
          in board |> has_won Piece.A =~ is false'
        end;
        it "is true for NE line of 4" begin
          let board = build [
            "---A---";
            "--AB---";
            "-ABB---";
            "ABBB---"]
          in board |> has_won Piece.A =~ is true'
        end;
        it "is false for NW line of 3" begin
          let board = build [
            "A------";
            "BA-----";
            "BBA----"]
          in board |> has_won Piece.A =~ is false'
        end;
        it "is true for NW line of 4" begin
          let board = build [
            "A------";
            "BA-----";
            "BBA----";
            "BBBA---"]
          in board |> has_won Piece.A =~ is true'
        end;
        it "is false when there is a gap in the line" begin
          let board = build [
            "----B--";
            "---BB--";
            "--BAA--";
            "--AAA--";
            "B-AAA--"]
          in board |> has_won Piece.B =~ is false'
        end;
      ];
      describe ".to_string" [
        it "for empty board is 6 row by 7 cols of -" begin
          let expected = (
            "- - - - - - -\n" ^
            "- - - - - - -\n" ^
            "- - - - - - -\n" ^
            "- - - - - - -\n" ^
            "- - - - - - -\n" ^
            "- - - - - - -\n" )
          in empty |> to_string =~ is (equal_to_string expected)
        end;
      ];
    ] end;

    describe "Game.play_turn" [
      it "uses function passed in to get row to drop in" begin
        let col = ref None in
        let module TestGame = Game.Make (struct include Board
          let drop _ c board =
            col := Some c;
            board
        end) in

        let players = Player.create_pair (drop_in 3, drop_in 0) in

        TestGame.create players |> TestGame.play_turn |> ignore;

        let equal_to_int_option = equal_to (function None -> "None" | Some n -> "Some " ^ (string_of_int n)) in

        !col =~ is (equal_to_int_option (Some 3))
      end;
      it "calls drop handler" begin
        let drop_handled = ref false in
        let handler = function (1,3,Piece.A) -> drop_handled := true | _ -> () in

        Player.create_pair (drop_in 3, drop_in 2) |> TestGame.create |> TestGame.on_drop handler |> 
        TestGame.play_turn |> ignore;

        !drop_handled =~ is true'
      end;
      it "calls switch player handler" begin
        let switch_handled = ref false in
        let handler = function Piece.B -> switch_handled := true | _ -> () in

        Player.create_pair (drop_in 1, drop_in 2) |> TestGame.create |> TestGame.on_switch handler |> 
        TestGame.play_turn |> ignore;

        !switch_handled =~ is true'
      end;
      it "calls win handler" begin
        let win_handled = ref false in
        let handler = function Piece.A -> win_handled := true | _ -> () in
        let play_turn = TestGame.play_turn in
        let game = Player.create_pair (drop_in 1, drop_in 2) |> TestGame.create |> TestGame.on_win handler |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn in

        game |> play_turn |> ignore;

        !win_handled =~ is true'
      end;
      it "toggles current piece" begin
        let history = ref [] in
        let handler a = history := (a :: !history) in
        Player.create_pair (drop_in 3, drop_in 2) |> TestGame.create |> TestGame.on_switch handler |>

        TestGame.play_turn |> TestGame.play_turn |> TestGame.play_turn |> ignore;

        let equal_to_piece_list = equal_to (List.fold_left (fun str piece -> str ^ "," ^ (Piece.to_string piece)) "") in

        !history =~ is (equal_to_piece_list [Piece.B;Piece.A;Piece.B])
      end;
    ];

    describe "AI" [
      describe ".minimax" begin
        let equal_to_float = within 0.0001 in [
          
          it "returns 0 if empty" begin
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
                | "- A - - - - -" ->  5.
                | _         ->  0.
            end) in

            TestAI.minimax 1 Piece.A Piece.A TestAI.winning_score Board.empty =~ is (equal_to_float 5.)
          end;
          it "with depth 2 returns highest of lowest eval values after 2 turns" begin
            let module TestAI = Ai.Make (struct include Board
              let evaluate player board =
                let bottom_row = board |> to_string |> lines |> List.last in
                let col_with piece = 
                  try Str.search_forward (Str.regexp (piece |> Piece.to_string)) bottom_row 0 |> flip (lsr) 1 |> (+) 1
                  with Not_found -> 0
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
            let module Board = struct include Board
              let evaluate _ board =
                match board |> to_string |> lines |> List.last with
                | "- - - A - - -" -> 1.
                | _         -> 0.
            end in
            let module TestAI = Ai.Make (Board) in

            TestAI.choose_column 0 Board.empty Piece.A =~ is (equal_to_int 3)
          end;
        ];
      ];
  ]
