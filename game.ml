open Player
open Board
open Util

type event = Drop of (int * int * player) | Switch of player | Win of player

type game = Game of (player * player) * board * (event -> unit) list

let new_game = Game ((A, B), empty_board, [])

let play_turn move game =
  match game with | Game ((a,b), board, handlers) ->
    let col = (move game) in
    let board = drop a col board in
    let row = top col board in
    let fire event =
      List.iter ((|>) event) handlers in
    fire (Drop (row,col,a));
    fire (Switch b);
    if wins a board then fire (Win a);
    (Game ((b,a), board, handlers)) 

let handle handler game =
  match game with | Game ((a,b), board, handlers) -> Game ((a,b), board, handler::handlers)

let string_of_game game =
  match game with | Game (_, board, _) -> string_of_board board
