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
      "string_of_board for empty board is 6 row by 7 cols of -" >:: (fun() ->
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
      let move c _ = (col := Some c; c) in
      Game.create Player.A Player.B |> Game.play_turn (move 3) |> ignore;
      assert_equal !col (Some 3)
    );
    "play_turn calls drop handler" >:: (fun() ->
      let game = Game.create Player.A Player.B in
      let handled = ref false in
      let handler = function Game.Drop (1,3,Player.A) -> handled := true | _ -> () in
      let game = Game.handle handler game in
      game |> Game.play_turn move |> ignore;
      assert_equal !handled true
    );
    "play_turn calls switch player handler" >:: (fun() ->
      let game = Game.create Player.A Player.B in
      let handled = ref false in
      let handler = function Game.Switch Player.B -> handled := true | _ -> () in
      let game = Game.handle handler game in
      game |> Game.play_turn move |> ignore;
      assert_equal !handled true
    );
    "play_turn calls win handler" >:: (fun() ->
      let first _ = 1 and second _ = 2 in
      let game = Game.create Player.A Player.B |> 
        Game.play_turn first |> Game.play_turn second |> 
        Game.play_turn first |> Game.play_turn second |> 
        Game.play_turn first |> Game.play_turn second in
      let win_handled = ref false in
      let handler = function Game.Win Player.A -> win_handled := true | _ -> () in
      let game = Game.handle handler game in
      game |> Game.play_turn first |> ignore;
      assert_equal !win_handled true
    );
    "play_turn toggles current player" >:: (fun() ->
      let game = Game.create Player.A Player.B in
      let players = ref [] in
      let handler = function Game.Switch a -> players := (a :: !players) | _ -> () in
      game |> Game.handle handler |> Game.play_turn move |> Game.play_turn move |> Game.play_turn move |> ignore;
      assert_equal !players [Player.B;Player.A;Player.B];
    );
  ]) 
