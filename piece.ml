type t = A | B

let to_string = function A -> "A" | B -> "B"
let of_string = function "A" -> A | "B" -> B | s -> failwith (Printf.sprintf "invalid piece: %s" s)

let opponent = function
  | A -> B
  | B -> A
