open Player

type event = Drop of (int * int * player) | Switch of player | Win of player

type game

val new_game : game

val play_turn : (game -> int) -> game -> game

val handle : (event -> unit) -> game -> game

val string_of_game : game -> string
