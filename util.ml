open List

let (|>) x f = f x
let (>>) f g x = g (f x)

let is_some = function Some _ -> true | None -> false

let (|->) start finish =
  let rec range current = 
    if current >= finish then [] else (current :: range (current+1))
  in
  range start

let list_of count value = Array.make count value |> Array.to_list

let transpose len matrix =
  let cross_section vectors i = map (fun vector -> 
    try nth vector i with Failure "nth" -> None
  ) vectors
  in map (cross_section matrix) (0 |-> len)

let sublist xs start finish = 
  let finish = min finish (length xs) in
  let start = min start finish in
  Array.sub (Array.of_list xs) start (finish-start) |> Array.to_list

let rotate_left vector i = sublist vector i (length vector) @ sublist vector 0 i
let rotate_right vector i = rev (rotate_left (rev vector) i)

let foldi func seed count = fold_left func seed (0 |-> count)
let mapi func vector = map2 func vector (0 |-> (length vector))
