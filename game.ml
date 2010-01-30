module Make (Board : sig
  type t 
  val empty : t
  val drop : Player.t -> int -> t -> t
  val wins : Player.t -> t -> bool 
  val top_row : int -> t -> int
  val to_string : t -> string
end) = struct
  type event = Drop of (int * int * Player.t) | Switch of Player.t | Win of Player.t

  type t = { current_player : Player.t; other_player : Player.t; board : Board.t; event_handlers : (event -> unit) list }

  let create player_1 player_2 = { 
    current_player = player_1; 
    other_player = player_2; 
    board = Board.empty; 
    event_handlers = [] 
  }

  let play_turn move game = match game with
    | { current_player = a; other_player = b; board = board; event_handlers = handlers } ->
      let col = (move game) in
      let new_board = Board.drop a col board in
      let row = Board.top_row col new_board in
      let fire event =
        List.iter ((|>) event) handlers in
      fire (Drop (row,col,a));
      fire (Switch b);
      if Board.wins a new_board then fire (Win a);
      { game with current_player = b; other_player = a; board = new_board }

  let handle handler game = match game with
    | { event_handlers = handlers } -> { game with event_handlers = handler::handlers }

  let to_string = function
    | { board = board } -> Board.to_string board
end

module Normal = Make (Board)
