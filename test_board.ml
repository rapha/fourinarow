open OUnit

open Board

(* tests --------------------------------------------------------- *)

let drop_in_each player board i = drop board player (i+1)
let drop_in i player board _ = drop board player (i+1)

let _ =
  run_test_tt ("drop" >::: [
      "empty is empty" >:: (fun() -> 
        assert_equal empty_board empty_board
      );
      "drop in first column puts player in first row of first column" >:: (fun() -> 
        (drop empty_board A 1) |> assert_equal (Board [[Some A]; []; []; []; []; []; []]) 
      );
      "drop in second column puts player in first row of second column" >:: (fun() ->
        (drop empty_board A 2) |> assert_equal (Board [[]; [Some A]; []; []; []; []; []])
      );
      "drop in last column puts player in first row of last column" >:: (fun() ->
        (drop empty_board A row_length) |> assert_equal (Board [[]; []; []; []; []; []; [Some A]])
      ); 
      "dropping in all columns puts player in first row of every column" >:: (fun () ->
        let board = foldi (drop_in_each A) empty_board row_length in
        board |> assert_equal (Board [[Some A]; [Some A]; [Some A]; [Some A]; [Some A]; [Some A]; [Some A]]) 
      );
      "drop twice in the same column and puts player in first and second row of first column" >:: (fun() ->
        (drop (drop empty_board A 1) B 1) |> assert_equal (Board [[Some A;Some B]; []; []; []; []; []; []]) 
      );
      "drop twice in the same column puts player in first and second row of first column" >:: (fun() ->
        let board_with_col_1_full = (Board ([[Some A;Some A;Some A;Some A;Some A;Some A]; [];[];[];[];[];[]])) in
        (fun() -> drop board_with_col_1_full A 1) |> assert_raises (Failure "column full") 
      );
  ]) |> (fun _ -> ())
  ;
  run_test_tt ("wins" >::: [
      "false for empty" >:: (fun() -> 
        wins empty_board A |> assert_equal false
      );
      "false for vertical line of 3" >:: (fun() -> 
        let board = foldi (drop_in 1 A) empty_board 3 in
        wins board A |> assert_equal false
      );
      "true for vertical line of 4" >:: (fun() -> 
        let board = foldi (drop_in 1 A) empty_board 4 in
        wins board A |> assert_equal true
      );
      "false for horizontal line of 3" >:: (fun() -> 
        let board = foldi (drop_in_each A) empty_board 3 in
        wins board A |> assert_equal false
      );
      "true for horizontal line of 4" >:: (fun() -> 
        let board = foldi (drop_in_each A) empty_board 4 in
        wins board A |> assert_equal true
      );
      "false for NE line of 3" >:: (fun() ->
        let board = foldi (fun g i ->
          let bs = foldi (drop_in i B) g i in
          drop bs A (i+1)
        ) empty_board 3 in
        wins board A |> assert_equal false
      );
      "false for NE line of 4" >:: (fun() ->
        let board = foldi (fun g i ->
          let bs = foldi (drop_in i B) g i in
          drop bs A (i+1)
        ) empty_board 4 in
        wins board A |> assert_equal true
      );
      "false for NW line of 3" >:: (fun() ->
        let board = foldi (fun g i ->
          let col = 3 - i in
          let bs = foldi (drop_in col B) g i in
          drop bs A (col+1)
        ) empty_board 3 in
        wins board A |> assert_equal false
      );
      "false for NW line of 3" >:: (fun() ->
        let board = foldi (fun g i ->
          let col = 4 - i in
          let bs = foldi (drop_in col B) g i in
          drop bs A (col+1)
        ) empty_board 4 in
        wins board A |> assert_equal true
      );
  ]) 
  
