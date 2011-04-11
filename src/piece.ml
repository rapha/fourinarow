type t = Yellow | Red 

let to_string = function
  | Yellow -> "Yellow" 
  | Red -> "Red"

let to_char = function
  | Yellow -> 'Y'
  | Red -> 'R'

let of_char = function 
  | 'Y' -> Yellow 
  | 'R' -> Red 
  | ch -> failwith (Printf.sprintf "invalid piece: %c" ch)

let opponent = function
  | Red -> Yellow
  | Yellow -> Red
