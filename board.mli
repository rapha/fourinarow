exception Column_full of Col.t
type t
val empty : t
val drop : Piece.t -> Col.t -> t -> t
val has_won : Piece.t -> t -> bool
val evaluate : Piece.t -> t -> float

val of_string : string -> t
