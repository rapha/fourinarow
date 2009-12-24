type t 

val empty : t

val drop : Player.t -> int -> t -> t

val wins : Player.t -> t -> bool 

val to_string : t -> string

val top_row : int -> t -> int
