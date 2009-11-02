open Player

type event = Drop of int | Switch of (player * player)

type game

val new_game : game

val play_turn : (game -> int) -> game -> game

val handle : (event -> unit) -> game -> game
