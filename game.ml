
open Batteries

module Make (Board : sig
  type t = Board.t
  val empty : t
  val drop : Piece.t -> int -> t -> t
  val has_won : Piece.t -> t -> bool 
  val top_row : int -> t -> int
  val to_string : t -> string
end) : sig
  type t
  val create : (Player.t * Player.t) -> t
  val play_turn : t -> t
  val on_win : (Piece.t -> unit) -> t -> t
  val on_switch : (Piece.t -> unit) -> t -> t
  val on_drop : ((int * int * Piece.t) -> unit) -> t -> t
  val to_string : t -> string
end = struct

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
      let piece_a, piece_b = Player.piece a, Player.piece b in
      let col = a |> Player.next_move board in
      let new_board = Board.drop piece_a col board in
      let row = Board.top_row col new_board in
      let fire event =
        List.iter ((|>) event) handlers in
      fire (Drop (row,col,piece_a));
      fire (Switch piece_b);
      if Board.has_won piece_a new_board then fire (Win piece_a);
      { game with current_player = b; other_player = a; board = new_board }

  let handle handler game = match game with
    | { event_handlers = handlers } -> { game with event_handlers = handler::handlers }

  let on_drop   handler = handle (function Drop args -> handler args | _ -> ())
  let on_win    handler = handle (function Win args -> handler args | _ -> ())
  let on_switch handler = handle (function Switch args -> handler args | _ -> ())

  let to_string = function
    | { board = board } -> Board.to_string board

end
