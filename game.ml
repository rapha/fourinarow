type event = Drop of (int * int * Player.t) | Switch of Player.t | Win of Player.t

type t = Game of (Player.t * Player.t) * Board.t * (event -> unit) list

let create player_1 player_2 = Game ((player_1, player_2), Board.empty, [])

let play_turn move game =
  match game with | Game ((a,b), board, handlers) ->
    let col = (move game) in
    let board = Board.drop a col board in
    let row = Board.top_row col board in
    let fire event =
      List.iter ((|>) event) handlers in
    fire (Drop (row,col,a));
    fire (Switch b);
    if Board.wins a board then fire (Win a);
    (Game ((b,a), board, handlers)) 

let handle handler = function 
  | (Game ((a,b), board, handlers)) -> Game ((a,b), board, handler::handlers)

let to_string = function
  | (Game (_, board, _)) -> Board.to_string board
