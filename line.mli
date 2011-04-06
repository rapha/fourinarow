exception Line_full

type 'a t

val create : 'a -> 'a -> 'a -> 'a -> 'a t
val fill : 'a -> 'a t -> 'a t
val is_full : 'a t -> bool
val includes: 'a -> 'a t -> bool
