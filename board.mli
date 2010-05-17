exception Column_full of int
type t
val empty : t
val drop : Piece.t -> int -> t -> t
val has_won : Piece.t -> t -> bool
val top_row : int -> t -> int
val to_string : t -> string
val build : string list -> t
val evaluate : Piece.t -> t -> float
