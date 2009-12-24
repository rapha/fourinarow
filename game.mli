type event = Drop of (int * int * Player.t) | Switch of Player.t | Win of Player.t

type t 

val create : Player.t -> Player.t -> t

val play_turn : (t -> int) -> t -> t

val handle : (event -> unit) -> t -> t

val to_string : t -> string
