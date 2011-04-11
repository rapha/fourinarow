type t = { piece : Piece.t; move : (Board.t -> Piece.t -> Col.t) }

let create_pair (strategy1, strategy2) =
  ({piece = Piece.Yellow; move = strategy1}, {piece = Piece.Red; move = strategy2})

let piece { piece = piece } = piece

let next_move board { piece = piece; move = move } =
  move board piece

