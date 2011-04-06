exception Column_full of Col.t
type t
val empty : t
val drop : Piece.t -> Col.t -> t -> (t * Row.t)
val has_won : Piece.t -> t -> bool
val to_string : t -> string
val of_string : string -> t
val evaluate : Piece.t -> t -> float
