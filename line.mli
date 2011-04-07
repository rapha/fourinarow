exception Line_full

type 'cell t

val create : 'cell -> 'cell -> 'cell -> 'cell -> 'cell t
val fill : 'cell -> 'cell t -> 'cell t
val is_full : 'cell t -> bool
val includes: 'cell -> 'cell t -> bool
