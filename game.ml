open Player
open Board

type event = Drop of int | Switch of (player * player)

type game = Game of (player * player) * board * (event -> unit) list

let new_game = Game ((A, B), empty_board, [])

let play_turn move game =
  match game with | Game ((a,b), board, handlers) ->
    let col = (move game) in
    let board = drop a col board in
    List.iter (fun handler -> handler (Drop col)) handlers;
    List.iter (fun handler -> handler (Switch (b,a))) handlers;
    (Game ((b,a), board, handlers)) 

let handle handler game =
  match game with | Game ((a,b), board, handlers) -> Game ((a,b), board, handler::handlers)
