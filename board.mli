type player

val player_a : player
val player_b : player

type board

val row_length : int
val col_length : int

val empty_board : board

val drop : board -> player -> int -> board

val wins : board -> player -> bool 
