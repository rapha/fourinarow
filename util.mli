val foldi : ('a -> int -> 'a) -> 'a -> int -> 'a
val mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
val list_of : int -> 'a -> 'a list
val transpose : int -> 'a option list list -> 'a option list list
val sublist : int -> int -> 'a list -> 'a list
val rotate_left : int -> 'a list -> 'a list
val rotate_right : int -> 'a list -> 'a list
val tap : ('a -> 'b) -> 'a -> 'a
