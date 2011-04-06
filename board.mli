exception Column_full of Col_index.t
type t
val empty : t
val drop : Piece.t -> Col_index.t -> t -> (t * Row_index.t)
val has_won : Piece.t -> t -> bool
val to_string : t -> string
val of_string : string -> t
val evaluate : Piece.t -> t -> float
