open Batteries

module TestGame = Game.Make(Board)

open OUnit

let _ =
  let lines = Str.split (Str.regexp "\n") in
  let drop_in col _ _ = col in

  run_test_tt_main ("unit tests" >::: [
    ("Player.to_string and Player.of_string are inverse" >::: [
        "piece -> string -> piece" >:: (fun() ->
          Piece.A |> Piece.to_string |> Piece.of_string |> assert_equal Piece.A
        );
        "string -> piece -> string" >:: (fun() ->
          "A" |> Piece.of_string |> Piece.to_string |> assert_equal "A"
        );
    ]);
    ("Board" >::: [
      ".drop" >::: [
        "in an empty column will put a piece on the first row of that column" >:: (fun() ->
          let expected = (Board.build [
            "A------";
          ]) |> Board.to_string in

          Board.empty |> Board.drop Piece.A 0 |> Board.to_string |> assert_equal expected ~printer:identity
        );
      ];
      ".has_won" >::: [
        "is false for empty" >:: (fun() ->
          Board.empty |> Board.has_won Piece.A |> assert_equal false
        );
        "is false for vertical line of 3" >:: (fun() ->
          Board.build [
            "A------";
            "A------";
            "A------"]
            |> Board.has_won Piece.A |> assert_equal false
        );
        "is true for vertical line of 4" >:: (fun() ->
          Board.build [
            "A------";
            "A------";
            "A------";
            "A------"]
          |> Board.has_won Piece.A |> assert_equal true
        );
        "is false for horizontal line of 3" >:: (fun() ->
          Board.build [
            "AAA----"]
          |> Board.has_won Piece.A |> assert_equal false
        );
        "is true for horizontal line of 4" >:: (fun() ->
          Board.build [
            "AAAA---"]
          |> Board.has_won Piece.A |> assert_equal true
        );
        "is false for NE line of 3" >:: (fun() ->
          Board.build [
            "--A----";
            "-AB----";
            "ABB----"]
          |> Board.has_won Piece.A |> assert_equal false
        );
        "is true for NE line of 4" >:: (fun() ->
          Board.build [
            "---A---";
            "--AB---";
            "-ABB---";
            "ABBB---"]
          |> Board.has_won Piece.A |> assert_equal true
        );
        "is false for NW line of 3" >:: (fun() ->
          Board.build [
            "A------";
            "BA-----";
            "BBA----"]
          |> Board.has_won Piece.A |> assert_equal false
        );
        "is true for NW line of 4" >:: (fun() ->
          Board.build [
            "A------";
            "BA-----";
            "BBA----";
            "BBBA---"]
          |> Board.has_won Piece.A |> assert_equal true
        );
        "is false when there is a gap in the line" >:: (fun() ->
          Board.build [
            "----B--";
            "---BB--";
            "--BAA--";
            "--AAA--";
            "B-AAA--"]
          |> Board.has_won Piece.B |> assert_equal false
        );
      ];
      (".to_string" >::: [
        "for empty board is 6 row by 7 cols of -" >:: (fun() ->
          Board.empty |> Board.to_string |> assert_equal (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ) 
            ~printer:(identity)
        );
      ]);
    ]);


    ("Game.play_turn" >::: [
      "uses function passed in to get row to drop in" >:: (fun() ->
        let col = ref None in
        let module TestGame = Game.Make (struct include Board
          let drop _ c board =
            col := Some c;
            board
        end) in

        let players = Player.create_pair (drop_in 3, drop_in 0) in

        TestGame.create players |> TestGame.play_turn |> ignore;

        assert_equal !col (Some 3)
      );
      "calls drop handler" >:: (fun() ->
        let drop_handled = ref false in
        let handler = function (1,3,Piece.A) -> drop_handled := true | _ -> () in

        Player.create_pair (drop_in 3, drop_in 2) |> TestGame.create |> TestGame.on_drop handler |> 
        TestGame.play_turn |> ignore;

        assert_equal !drop_handled true
      );
      "calls switch player handler" >:: (fun() ->
        let switch_handled = ref false in
        let handler = function Piece.B -> switch_handled := true | _ -> () in

        Player.create_pair (drop_in 1, drop_in 2) |> TestGame.create |> TestGame.on_switch handler |> 
        TestGame.play_turn |> ignore;

        assert_equal !switch_handled true
      );
      "calls win handler" >:: (fun() ->
        let win_handled = ref false in
        let handler = function Piece.A -> win_handled := true | _ -> () in
        let play_turn = TestGame.play_turn in
        let game = Player.create_pair (drop_in 1, drop_in 2) |> TestGame.create |> TestGame.on_win handler |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn |>
                                      play_turn |> play_turn in

        game |> play_turn |> ignore;

        assert_equal !win_handled true
      );
      "toggles current piece" >:: (fun() ->
        let history = ref [] in
        let handler a = history := (a :: !history) in
        Player.create_pair (drop_in 3, drop_in 2) |> TestGame.create |> TestGame.on_switch handler |>

        TestGame.play_turn |> TestGame.play_turn |> TestGame.play_turn |> ignore;

        assert_equal !history [Piece.B;Piece.A;Piece.B];
      );
    ]);

    ("AI" >::: [
      ".minimax" >::: [
        
        "returns 0 if empty" >:: (fun() ->
          let module TestAI = Ai.Make(Board) in

          Board.empty |> TestAI.minimax 0 Piece.A Piece.A TestAI.winning_score |>
          assert_equal 0. ~printer:string_of_float
        );
        "returns losing score if opponent has won" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let has_won player board =
              player = Piece.B
          end) in

          TestAI.minimax 0 Piece.A Piece.A TestAI.winning_score Board.empty |>
          assert_equal TestAI.losing_score
        );
        "with depth 0 returns value from eval function" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let evaluate _ _ = 5.
          end) in

          TestAI.minimax 0 Piece.A Piece.A TestAI.winning_score Board.empty |>
          assert_equal 5.
          );
        "with depth 1 returns winning score if player can win this turn" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let has_won player board =
              player = Piece.A
          end) in

          TestAI.minimax 1 Piece.A Piece.A TestAI.winning_score Board.empty |>
          assert_equal TestAI.winning_score
          );
        "with depth 1 returns highest values from eval function after this turn" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let evaluate player board =
              match board |> to_string |> lines |> List.last with
              | "-A-----" ->  5.
              | _         ->  0.
          end) in

          TestAI.minimax 1 Piece.A Piece.A TestAI.winning_score Board.empty |>
          assert_equal 5. ~printer:string_of_float
          );
        "with depth 2 returns highest of lowest eval values after 2 turns" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let evaluate player board =
              let bottom_row = board |> to_string |> lines |> List.last in
              let col_with piece = 
                try Str.search_forward (Str.regexp (piece |> Piece.to_string)) bottom_row 0 |> (+) 1
                with Not_found -> 0
              in
              (col_with Piece.A) + (col_with Piece.B) |> ( * ) (-1) |> float_of_int
          end) in

          TestAI.minimax 2 Piece.A Piece.A TestAI.winning_score Board.empty |>
          assert_equal (-8.) ~printer:string_of_float
          );
        "with depth 1 returns an full score value for a full column" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let drop _ col _ =
              raise (Board.Column_full col)
          end) in

          TestAI.minimax 1 Piece.A Piece.A TestAI.column_full_score Board.empty |>
          assert_equal TestAI.column_full_score
          );
        ];
        (".choose_column" >::: [
          "returns the move with the highest score" >:: (fun() ->
            let module Board = struct include Board
              let evaluate _ board =
                match board |> to_string |> lines |> List.last with
                | "---A---" -> 1.
                | _         -> 0.
            end in
            let module TestAI = Ai.Make (Board) in

            TestAI.choose_column 0 Board.empty Piece.A |>
            assert_equal 3 ~printer:string_of_int
            );
        ]);
      ]);
    ]) |> ignore;

