open Player

type board

val empty_board : board

val drop : player -> int -> board -> board

val wins : player -> board -> bool 

val string_of_board : board -> string

val top : int -> board -> int
