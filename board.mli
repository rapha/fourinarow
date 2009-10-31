type player = A | B

type board

val empty_board : board

val drop : board -> player -> int -> board

val wins : board -> player -> bool 
