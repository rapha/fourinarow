open Player
open Board
open Util

type event = Drop of (int * int * player) | Switch of player

type game = Game of (player * player) * board * (event -> unit) list

let new_game = Game ((A, B), empty_board, [])

let play_turn move game =
  match game with | Game ((a,b), board, handlers) ->
    let col = (move game) in
    let board = drop a col board in
    let row = top col board in
    List.iter (fun handler -> handler (Drop (row,col,a))) handlers;
    List.iter (fun handler -> handler (Switch b)) handlers;
    (Game ((b,a), board, handlers)) 

let handle handler game =
  match game with | Game ((a,b), board, handlers) -> Game ((a,b), board, handler::handlers)

let string_of_game game =
  match game with | Game (_, board, _) -> string_of_board board

let winner game =
  match game with | Game ((a,b), board, _) -> 
    if wins a board then Some a else if wins b board then Some b else None
