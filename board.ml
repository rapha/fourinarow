open OUnit
open List

(* misc util functions ------------------------------------------- *)

let (|>) x f = f x
let is_some = function Some _ -> true | None -> false

(* list functions ------------------------------------------- *)

let (|->) start finish =
  let rec range current = 
    if current >= finish then [] else (current :: range (current+1))
  in
  range start

let list_of count value = Array.make count value |> Array.to_list

let transpose matrix =
  let cross_section vectors i = map (fun vector -> 
    try nth vector i with Failure "nth" -> None
  ) vectors
  in map (cross_section matrix) (0 |-> (length matrix))

let sublist xs start finish = 
  let finish = min finish (length xs) in
  let start = min start finish in
  Array.sub (Array.of_list xs) start (finish-start) |> Array.to_list

let rotate_left vector i = sublist vector i (length vector) @ sublist vector 0 i
let rotate_right vector i = rev (rotate_left (rev vector) i)

let mapi func vector = map2 func vector (0 |-> (length vector))
let foldi func seed count = fold_left func seed (0 |-> count)

(* domain code ------------------------------------------- *)

type player = A | B | C

type board = Board of player option list list
let row_length, col_length = 7, 6

let empty_board = Board (list_of row_length [])

let columns board = match board with Board cols -> cols
let rows board = columns board |> transpose
  
let string_of_board board = 
  let string_of_player player = match player with Some A -> "A" | Some B -> "B" | Some C -> "C" | None -> "-" in
  let string_of_row row = map string_of_player row |> fold_left (^) "" in
  rows board |> rev |> map string_of_row |> fold_left (fun sofar line -> sofar ^ line ^ "\n") ""

let tilt_left matrix = matrix |> map (fun row -> row @ list_of (col_length-1) None) |> mapi rotate_right
let tilt_right matrix = matrix |> map (fun row -> list_of (col_length-1) None @ row) |> mapi rotate_left

let diagonals tilt board = 
  rows board |> 
  rev |> 
  tilt |>
  transpose |> 
  map (filter is_some)

let north_east = diagonals tilt_left
let north_west = diagonals tilt_right

let drop board player col = 
  let cols = columns board in
  let column = nth cols (col-1) in
  if (length column >= col_length) then failwith "column full" else ();
  let new_column = column @ [Some player] and
      before = (sublist cols 0   (col-1)) and
      after  = (sublist cols col (length cols)) 
  in Board (before @ [new_column] @ after)

let has_four_in_a_row player lines =
  let four_in_a_row = list_of 4 (Some player)
  and contains_sublist sub_list full_list =
    let rec rest_contains_sublist sub rest =
      match (sub, rest) with 
        | ([], _) -> true
        | (_, []) -> false
        | (x::sub_tail, y::rest_tail) when x = y -> rest_contains_sublist sub_tail rest_tail
        | (_, y::rest_tail) -> rest_contains_sublist sub_list rest_tail
    in rest_contains_sublist sub_list full_list
  in lines |> exists (contains_sublist four_in_a_row)

let wins board player = 
  [columns; rows; north_east; north_west] 
    |> map ((|>) board)
    |> exists (has_four_in_a_row player)

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
  
