type t = A | B

let to_string = function A -> "A" | B -> "B"
let of_string = function "A" -> Some A | "B" -> Some B | _ -> None

let name = to_string

let eq (a:t) (b:t) = a = b
