type 'a t

exception Column_full

val empty : 'a t

(* 
 * Drop a piece into a column.
 * Returns the new columns and the row that the piece landed in
 * or raises Column_full if the column is full
 *)
val append : Col.t -> 'a -> 'a t -> ('a t * Row.t)

(** 
 * @param piece_to_string
 * @param columns
 * @return as_string
 *)
val to_string : ('a -> string) -> 'a t -> string
(**
 * @param piece_of_string
 * @param as_string
 * @return columns
 *)
val of_string : (string -> 'a) -> string -> 'a t
