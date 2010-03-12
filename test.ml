module TestGame = Game.Normal
module Board = Board.Make(Player)

open OUnit

let _ =
  let lines = Str.split (Str.regexp "\n") in

  run_test_tt_main ("unit tests" >::: [
    ("Player.to_string" >::: [
        "A" >:: (fun() ->
          Player.A |> Player.to_string |> assert_equal "A"
        );
        "B" >:: (fun() ->
          Player.B |> Player.to_string |> assert_equal "B"
        );
    ]);
    ("Board" >::: [
      ".wins" >::: [
        "is false for empty" >:: (fun() ->
          Board.empty |> Board.wins Player.A |> assert_equal false
        );
        "is false for vertical line of 3" >:: (fun() ->
          Board.build [
            "A------";
            "A------";
            "A------"]
            |> Board.wins Player.A |> assert_equal false
        );
        "is true for vertical line of 4" >:: (fun() ->
          Board.build [
            "A------";
            "A------";
            "A------";
            "A------"]
          |> Board.wins Player.A |> assert_equal true
        );
        "is false for horizontal line of 3" >:: (fun() ->
          Board.build [
            "AAA----"]
          |> Board.wins Player.A |> assert_equal false
        );
        "is true for horizontal line of 4" >:: (fun() ->
          Board.build [
            "AAAA---"]
          |> Board.wins Player.A |> assert_equal true
        );
        "is false for NE line of 3" >:: (fun() ->
          Board.build [
            "--A----";
            "-AB----";
            "ABB----"]
          |> Board.wins Player.A |> assert_equal false
        );
        "is true for NE line of 4" >:: (fun() ->
          Board.build [
            "---A---";
            "--AB---";
            "-ABB---";
            "ABBB---"]
          |> Board.wins Player.A |> assert_equal true
        );
        "is false for NW line of 3" >:: (fun() ->
          Board.build [
            "A------";
            "BA-----";
            "BBA----"]
          |> Board.wins Player.A |> assert_equal false
        );
        "is true for NW line of 4" >:: (fun() ->
          Board.build [
            "A------";
            "BA-----";
            "BBA----";
            "BBBA---"]
          |> Board.wins Player.A |> assert_equal true
        );
        "is false when there is a gap in the line" >:: (fun() ->
          Board.build [
            "----B--";
            "---BB--";
            "--BAA--";
            "--AAA--";
            "B-AAA--"]
          |> Board.wins Player.B |> assert_equal false
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

        TestGame.create Player.A Player.B |> TestGame.play_turn (fun _ -> 3) |> ignore;

        assert_equal !col (Some 3)
      );
      "calls drop handler" >:: (fun() ->
        let drop_handled = ref false in
        let handler = function TestGame.Drop (1,3,Player.A) -> drop_handled := true | _ -> () in
        let game = TestGame.create Player.A Player.B |> TestGame.handle handler in

        game |> TestGame.play_turn (fun _ -> 3) |> ignore;

        assert_equal !drop_handled true
      );
      "calls switch player handler" >:: (fun() ->
        let switch_handled = ref false in
        let handler = function TestGame.Switch Player.B -> switch_handled := true | _ -> () in
        let game = TestGame.create Player.A Player.B |> TestGame.handle handler in

        game |> TestGame.play_turn (fun _ -> 1) |> ignore;

        assert_equal !switch_handled true
      );
      "calls win handler" >:: (fun() ->
        let win_handled = ref false in
        let handler = function TestGame.Win Player.A -> win_handled := true | _ -> () in
        let first_col _ = 1 and second_col _ = 2 in
        let drop_in = TestGame.play_turn in
        let game = TestGame.create Player.A Player.B |> TestGame.handle handler |>
          drop_in first_col |> drop_in second_col |>
          drop_in first_col |> drop_in second_col |>
          drop_in first_col |> drop_in second_col in

        game |> drop_in first_col |> ignore;

        assert_equal !win_handled true
      );
      "toggles current player" >:: (fun() ->
        let player_history = ref [] in
        let handler = function TestGame.Switch a -> player_history := (a :: !player_history) | _ -> () in
        let game = TestGame.create Player.A Player.B |> TestGame.handle handler in 
        let move _ = 3 in

        game |> TestGame.play_turn move |> TestGame.play_turn move |> TestGame.play_turn move |> ignore;

        assert_equal !player_history [Player.B;Player.A;Player.B];
      );
    ]);

    ("AI" >::: [
      ".minimax" >::: [
        "returns 0 if empty" >:: (fun() ->
          let module TestAI = Ai.Make(Board) in

          Board.empty |> TestAI.minimax 0 Player.A (Player.A,Player.B) TestAI.winning_score |>
          assert_equal 0.
        );
        "returns losing score if opponent has won" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let wins player board =
              player = Player.B
          end) in

          TestAI.minimax 0 Player.A (Player.A,Player.B) TestAI.winning_score Board.empty |>
          assert_equal TestAI.losing_score
        );
        "with depth 0 returns value from eval function" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let evaluate _ _ = 5.
          end) in

          TestAI.minimax 0 Player.A (Player.A, Player.B) TestAI.winning_score Board.empty |>
          assert_equal 5.
          );
        "with depth 1 returns winning score if player can win this turn" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let wins player board =
              player = Player.A
          end) in

          TestAI.minimax 1 Player.A (Player.A, Player.B) TestAI.winning_score Board.empty |>
          assert_equal TestAI.winning_score
          );
        "with depth 1 returns highest values from eval function after this turn" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let evaluate player board =
              match board |> to_string |> lines |> List.last with
              | "-A-----" ->  5.
              | _         ->  0.
          end) in

          TestAI.minimax 1 Player.A (Player.A, Player.B) TestAI.winning_score Board.empty |>
          assert_equal 5.
          );
        "with depth 2 returns highest of lowest eval values after 2 turns" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let evaluate player board =
              let bottom_row = board |> to_string |> lines |> List.last in
              let col_with player = 
                try Str.search_forward (Str.regexp (player |> Player.to_string)) bottom_row 0 |> (+) 1
                with Not_found -> 0
              in
              (col_with Player.A) + (col_with Player.B) |> ( * ) (-1) |> float_of_int
          end) in

          TestAI.minimax 2 Player.A (Player.A, Player.B) TestAI.winning_score Board.empty |>
          assert_equal (-8.)
          );
        "with depth 1 returns an full score value for a full column" >:: (fun() ->
          let module TestAI = Ai.Make (struct include Board
            let drop _ col _ =
              raise (Board.Column_full col)
          end) in

          TestAI.minimax 1 Player.A (Player.A, Player.B) TestAI.column_full_score Board.empty |>
          assert_equal TestAI.column_full_score
          );
        ];
        (".choose_column" >::: [
          "returns the move with the highest score" >:: (fun() ->
            let module TestAI = Ai.Make (struct include Board
              let evaluate _ board =
                match board |> to_string |> lines |> List.last with
                | "---A---" -> 1.
                | _         -> 0.
            end) in

            TestAI.choose_column 0 (TestGame.create Player.A Player.B) |>
            assert_equal ~printer:string_of_int 3
            );
        ]);
      ]);
    ]) |> ignore;

