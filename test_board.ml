open OUnit

open Util
open Board

let (|>) x f = f x

(* tests --------------------------------------------------------- *)

let drop_in_each player board i = drop board player (i+1)
let drop_in i player board _ = drop board player (i+1)

let _ =
  run_test_tt ("wins" >::: [
      "false for empty" >:: (fun() -> 
        wins empty_board player_a |> assert_equal false
      );
      "false for vertical line of 3" >:: (fun() -> 
        let board = foldi (drop_in 1 player_a) empty_board 3 in
        wins board player_a |> assert_equal false
      );
      "true for vertical line of 4" >:: (fun() -> 
        let board = foldi (drop_in 1 player_a) empty_board 4 in
        wins board player_a |> assert_equal true
      );
      "false for horizontal line of 3" >:: (fun() -> 
        let board = foldi (drop_in_each player_a) empty_board 3 in
        wins board player_a |> assert_equal false
      );
      "true for horizontal line of 4" >:: (fun() -> 
        let board = foldi (drop_in_each player_a) empty_board 4 in
        wins board player_a |> assert_equal true
      );
      "false for NE line of 3" >:: (fun() ->
        let board = foldi (fun g i ->
          let bs = foldi (drop_in i player_b) g i in
          drop bs player_a (i+1)
        ) empty_board 3 in
        wins board player_a |> assert_equal false
      );
      "false for NE line of 4" >:: (fun() ->
        let board = foldi (fun g i ->
          let bs = foldi (drop_in i player_b) g i in
          drop bs player_a (i+1)
        ) empty_board 4 in
        wins board player_a |> assert_equal true
      );
      "false for NW line of 3" >:: (fun() ->
        let board = foldi (fun g i ->
          let col = 3 - i in
          let bs = foldi (drop_in col player_b) g i in
          drop bs player_a (col+1)
        ) empty_board 3 in
        wins board player_a |> assert_equal false
      );
      "false for NW line of 3" >:: (fun() ->
        let board = foldi (fun g i ->
          let col = 4 - i in
          let bs = foldi (drop_in col player_b) g i in
          drop bs player_a (col+1)
        ) empty_board 4 in
        wins board player_a |> assert_equal true
      );
  ]) 
  
