open Player

type event = Drop of int | Switch of (player * player)

type game

val new_game : game

val play_turn : (game -> int) -> game -> game

val handle : (event -> unit) -> game -> game

val winner : game -> player option

val string_of_game : game -> string
