val (|>) : 'a -> ('a -> 'b) -> 'b
val (>>) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
val foldi : ('a -> int -> 'a) -> 'a -> int -> 'a
val is_some : 'a option -> bool
val mapi : ('a -> int -> 'b) -> 'a list -> 'b list
val list_of : int -> 'a -> 'a list
val transpose : 'a option list list -> 'a option list list
val sublist : 'a list -> int -> int -> 'a list
val rotate_left : 'a list -> int -> 'a list
val rotate_right : 'a list -> int -> 'a list
