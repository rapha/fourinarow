type t

val empty : t
val add : Piece.t -> (Row.t * Col.t) -> t -> t
val has_won : Piece.t -> t -> bool
