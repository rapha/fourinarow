type t 

val empty : t

val drop : Player.t -> int -> t -> t

val wins : Player.t -> t -> bool 

val top_row : int -> t -> int

val to_string : t -> string

val build : string list -> t
