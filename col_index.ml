open Batteries

type t = Col1 | Col2 | Col3 | Col4 | Col5 | Col6 | Col7

let left_to_right = [Col1; Col2; Col3; Col4; Col5; Col6; Col7]
let right_to_left = List.rev left_to_right

let to_int = function
  | Col1 -> 0
  | Col2 -> 1
  | Col3 -> 2
  | Col4 -> 3
  | Col5 -> 4
  | Col6 -> 5
  | Col7 -> 6

let of_int = function
  | 0 -> Col1
  | 1 -> Col2
  | 2 -> Col3
  | 3 -> Col4
  | 4 -> Col5
  | 5 -> Col6
  | 6 -> Col7
  | _ -> invalid_arg "col index must be in range [0-6]"

let to_string index = "Col" ^ (index |> to_int |> (+) 1 |> string_of_int)
