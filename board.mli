open Player

type board

val empty_board : board

val drop : player -> int -> board -> board

val wins : player -> board -> bool 
