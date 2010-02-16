module TestGame = Game.Normal

open OUnit

let _ =
  let move _ = 3 in

  run_test_tt ("Player" >::: [
      "to_string A" >:: (fun() ->
        Player.A |> Player.to_string |> assert_equal "A"
      );
      "to_string B" >:: (fun() ->
        Player.B |> Player.to_string |> assert_equal "B"
      );
  ]) |> ignore;
  run_test_tt ("Board.wins" >::: [
      "false for empty" >:: (fun() ->
        Board.empty |> Board.wins Player.A |> assert_equal false
      );
      "false for vertical line of 3" >:: (fun() ->
        Board.build [
          "A------";
          "A------";
          "A------"]
          |> Board.wins Player.A |> assert_equal false
      );
      "true for vertical line of 4" >:: (fun() ->
        Board.build [
          "A------";
          "A------";
          "A------";
          "A------"]
        |> Board.wins Player.A |> assert_equal true
      );
      "false for horizontal line of 3" >:: (fun() ->
        Board.build [
          "AAA----"]
        |> Board.wins Player.A |> assert_equal false
      );
      "true for horizontal line of 4" >:: (fun() ->
        Board.build [
          "AAAA---"]
        |> Board.wins Player.A |> assert_equal true
      );
      "false for NE line of 3" >:: (fun() ->
        Board.build [
          "--A----";
          "-AB----";
          "ABB----"]
        |> Board.wins Player.A |> assert_equal false
      );
      "true for NE line of 4" >:: (fun() ->
        Board.build [
          "---A---";
          "--AB---";
          "-ABB---";
          "ABBB---"]
        |> Board.wins Player.A |> assert_equal true
      );
      "false for NW line of 3" >:: (fun() ->
        Board.build [
          "A------";
          "BA-----";
          "BBA----"]
        |> Board.wins Player.A |> assert_equal false
      );
      "true for NW line of 4" >:: (fun() ->
        Board.build [
          "A------";
          "BA-----";
          "BBA----";
          "BBBA---"]
        |> Board.wins Player.A |> assert_equal true
      );
      "false when there is a gap in the line" >:: (fun() ->
        Board.build [
          "----B--";
          "---BB--";
          "--BAA--";
          "--AAA--";
          "B-AAA--"]
        |> Board.wins Player.B |> assert_equal false
      );
      "Board.to_string for empty board is 6 row by 7 cols of -" >:: (fun() ->
        Board.empty |> Board.to_string |> assert_equal (
          "-------\n" ^
          "-------\n" ^
          "-------\n" ^
          "-------\n" ^
          "-------\n" ^
          "-------\n" )
      );
  ]) |> ignore;

  run_test_tt ("game" >::: [
    "uses argument passed in to get row to drop in" >:: (fun() ->
      let col = ref None in
      let module TestGame = Game.Make (struct include Board
        let drop _ c board =
          col := Some c;
          board
      end) in

      TestGame.create Player.A Player.B |> TestGame.play_turn (fun _ -> 3) |> ignore;

      assert_equal !col (Some 3)
    );
    "play_turn calls drop handler" >:: (fun() ->
      let game = TestGame.create Player.A Player.B in
      let handled = ref false in
      let handler = function TestGame.Drop (1,3,Player.A) -> handled := true | _ -> () in
      let game = TestGame.handle handler game in
      game |> TestGame.play_turn move |> ignore;
      assert_equal !handled true
    );
    "play_turn calls switch player handler" >:: (fun() ->
      let game = TestGame.create Player.A Player.B in
      let handled = ref false in
      let handler = function TestGame.Switch Player.B -> handled := true | _ -> () in
      let game = TestGame.handle handler game in
      game |> TestGame.play_turn move |> ignore;
      assert_equal !handled true
    );
    "play_turn calls win handler" >:: (fun() ->
      let first _ = 1 and second _ = 2 in
      let game = TestGame.create Player.A Player.B |>
        TestGame.play_turn first |> TestGame.play_turn second |>
        TestGame.play_turn first |> TestGame.play_turn second |>
        TestGame.play_turn first |> TestGame.play_turn second in
      let win_handled = ref false in
      let handler = function TestGame.Win Player.A -> win_handled := true | _ -> () in
      let game = TestGame.handle handler game in
      game |> TestGame.play_turn first |> ignore;
      assert_equal !win_handled true
    );
    "play_turn toggles current player" >:: (fun() ->
      let game = TestGame.create Player.A Player.B in
      let players = ref [] in
      let handler = function TestGame.Switch a -> players := (a :: !players) | _ -> () in
      game |> TestGame.handle handler |> TestGame.play_turn move |> TestGame.play_turn move |> TestGame.play_turn move |> ignore;
      assert_equal !players [Player.B;Player.A;Player.B];
    );
  ]) |> ignore;

  run_test_tt ("AI" >::: [
      "minimax returns 0 if empty" >:: (fun() ->
        let module TestAI = Ai.Make(Board) in
        Board.empty |> TestAI.minimax 0 Player.A (Player.A,Player.B) TestAI.win |>
        assert_equal 0.
      );
      "minimax returns -Inf if opponent has won" >:: (fun() ->
        let module TestAI = Ai.Make (struct include Board
          let wins player board =
            player = Player.B
        end) in
        TestAI.minimax 0 Player.A (Player.A,Player.B) TestAI.win Board.empty |>
        assert_equal ~printer:string_of_float TestAI.lose
      );
      "minimax with depth 0 returns value from eval function" >:: (fun() ->
        let module TestAI = Ai.Make (struct include Board
          let evaluate _ _ = 5.
        end) in

        TestAI.minimax 0 Player.A (Player.A, Player.B) TestAI.win Board.empty |>
        assert_equal 5.
        );
      "minimax with depth 1 returns Inf if column makes player win" >:: (fun() ->
        let module TestAI = Ai.Make (struct include Board
          let wins player board =
            match board |> to_string |> Str.split (Str.regexp "\n") |> List.last with
            | "----A--" -> true
            | _         -> false
        end) in

        TestAI.minimax 1 Player.A (Player.A, Player.B) TestAI.win Board.empty |>
        assert_equal ~printer:string_of_float TestAI.win
        );
      "minimax with depth 1 returns highest values from eval function after 1 turn" >:: (fun() ->
        let module TestAI = Ai.Make (struct include Board
          let evaluate player board =
            match board |> to_string |> Str.split (Str.regexp "\n") |> List.last with
            | "-A-----" ->  5.
            | _         ->  0.
        end) in

        TestAI.minimax 1 Player.A (Player.A, Player.B) TestAI.win Board.empty |>
        assert_equal ~printer:string_of_float 5.
        );
      "minimax with depth 2 returns highest of lowest eval values after 2 turns" >:: (fun() ->
        let module TestAI = Ai.Make (struct include Board
          let evaluate player board =
            let bottom_row = board |> to_string |> Str.split (Str.regexp "\n") |> List.last in
            let col_with_A = Str.search_forward (Str.regexp "A") bottom_row 0 |> (+) 1 in
            col_with_A |> ( * ) (-1) |> float_of_int
        end) in

        TestAI.minimax 2 Player.A (Player.A, Player.B) TestAI.win Board.empty |>
        assert_equal ~printer:string_of_float (-1.)
        );
      "minimax returns an -Inf value for a full column" >:: (fun() ->
        let module TestAI = Ai.Make (Board) in

        TestAI.minimax 1 Player.A (Player.A, Player.B) TestAI.win (Board.build [
              "BABBBAB";
              "ABAAABA";
              "BABBBAB";
              "ABAAABA";
              "BABBBAB";
              "ABAAABA";
            ]) |>
        assert_equal TestAI.lose
        );
      "choose_column will return the move with the highest score" >:: (fun() ->
        let module TestAI = Ai.Make (struct include Board
          let evaluate player board =
            match board |> to_string |> Str.split (Str.regexp "\n") |> List.last with
            | "---A---" -> 1.
            | _         -> 0.
        end) in

        TestAI.choose_column 0 (TestGame.create Player.A Player.B) |>
        assert_equal 4
        );
  ]) |> ignore;

