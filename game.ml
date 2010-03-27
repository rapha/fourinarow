module Make (Board : sig
  type t = Board.t
  val empty : t
  val drop : Piece.t -> int -> t -> t
  val wins : Piece.t -> t -> bool 
  val top_row : int -> t -> int
  val to_string : t -> string
end) = struct

  type event = Drop of (int * int * Piece.t) | Switch of Piece.t | Win of Piece.t

  type t = { current_player : Player.t; other_player : Player.t; board : Board.t; event_handlers : (event -> unit) list }

  let create (player_1,player_2) = { 
    current_player = player_1; 
    other_player = player_2; 
    board = Board.empty; 
    event_handlers = [] 
  }

  let play_turn game = match game with
    | { current_player = a; other_player = b; board = board; event_handlers = handlers } ->
      let move = Player.move a in
      let piece_a, piece_b as pieces = Player.piece a, Player.piece b in
      let col = (move board pieces) in
      let new_board = Board.drop piece_a col board in
      let row = Board.top_row col new_board in
      let fire event =
        List.iter ((|>) event) handlers in
      fire (Drop (row,col,piece_a));
      fire (Switch piece_b);
      if Board.wins piece_a new_board then fire (Win piece_a);
      { game with current_player = b; other_player = a; board = new_board }

  let handle handler game = match game with
    | { event_handlers = handlers } -> { game with event_handlers = handler::handlers }

  let to_string = function
    | { board = board } -> Board.to_string board

  let board = function
    | { board = board } -> board

  let players = function
    | { current_player = current; other_player = other } -> (current, other)
end
