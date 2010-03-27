type t = { piece : Piece.t; move : (Board.t -> (Piece.t * Piece.t) -> int) }

let create_pair strategy1 strategy2 =
  ({piece = Piece.A; move = strategy1}, {piece = Piece.B; move = strategy2})

let piece { piece = piece } = piece
let move { move = move } = move
