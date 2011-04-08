type 'a t

exception Full

val empty : 'a t

(* 
 * Drop a piece into a column.
 * Returns the new columns and the row the piece landed in
 * or raises Full if the column is full
 *)
val append : Col.t -> 'a -> 'a t -> ('a t * Row.t)
