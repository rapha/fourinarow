open OUnit

open Util
open Board
open Game

open Player

let _ =
  let drop_in_each player board i = drop player (i+1) board in
  let drop_in i player board _ = drop player (i+1) board in
  let ignore _ = () in
  let move _ = 3 in

  run_test_tt ("player" >::: [
      "string_of_player A" >:: (fun() -> 
        A |> string_of_player |> assert_equal "A"
      );
      "string_of_player B" >:: (fun() -> 
        B |> string_of_player |> assert_equal "B"
      );
  ]) |> ignore;
  run_test_tt ("board wins" >::: [
      "false for empty" >:: (fun() -> 
        empty_board |> wins A |> assert_equal false
      );
      "false for vertical line of 3" >:: (fun() -> 
        foldi (drop_in 1 A) empty_board 3 |> wins A |> assert_equal false
      );
      "true for vertical line of 4" >:: (fun() -> 
        foldi (drop_in 1 A) empty_board 4 |> wins A |> assert_equal true
      );
      "false for horizontal line of 3" >:: (fun() -> 
        foldi (drop_in_each A) empty_board 3 |> wins A |> assert_equal false
      );
      "true for horizontal line of 4" >:: (fun() -> 
        foldi (drop_in_each A) empty_board 4 |> wins A |> assert_equal true
      );
      "false for NE line of 3" >:: (fun() ->
        foldi (fun g i ->
          foldi (drop_in i B) g i |> drop A (i+1)
        ) empty_board 3 
        |> wins A |> assert_equal false
      );
      "false for NE line of 4" >:: (fun() ->
        foldi (fun g i ->
          foldi (drop_in i B) g i |> drop A (i+1)
        ) empty_board 4
        |> wins A |> assert_equal true
      );
      "false for NW line of 3" >:: (fun() ->
        foldi (fun g i ->
          let col = 3 - i in
          foldi (drop_in col B) g i |> drop A (col+1)
        ) empty_board 3 
        |> wins A |> assert_equal false
      );
      "false for NW line of 3" >:: (fun() ->
        foldi (fun g i ->
          let col = 4 - i in
          foldi (drop_in col B) g i |> drop A (col+1)
        ) empty_board 4
        |> wins A |> assert_equal true
      );
  ]) |> ignore;
  
  run_test_tt ("game" >::: [
    "uses argument passed in to get row to drop in" >:: (fun() ->
      let col = ref None in
      let move c _ = (col := Some c; c) in
      new_game |> play_turn (move 3) |> ignore;
      assert_equal !col (Some 3)
    );
    "play_turn calls drop handler" >:: (fun() ->
      let game = new_game in
      let called = ref false in
      let handler = function Drop 3 -> called := true | _ -> () in
      let game = handle handler game in
      game |> play_turn move |> ignore;
      assert_equal !called true
    );
    "play_turn calls switch player handler" >:: (fun() ->
      let game = new_game in
      let called = ref false in
      let handler = function Switch (B,A) -> called := true | _ -> () in
      let game = handle handler game in
      game |> play_turn move |> ignore;
      assert_equal !called true
    );
    "play_turn toggles current player" >:: (fun() ->
      let game = new_game in
      let players = ref [] in
      let handler = function Switch (a,b) -> players := (a :: !players) | _ -> () in
      game |> handle handler |> play_turn move |> play_turn move |> play_turn move |> ignore;
      assert_equal !players [B;A;B];
    );
  ]) 
