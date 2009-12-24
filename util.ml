let list_of count value = Array.make count value |> Array.to_list

let transpose len matrix =
  let cross_section vectors i = List.map (fun vector -> 
    try List.nth vector i with Extlib.ExtList.List.Invalid_index _ -> None
  ) vectors
  in List.map (cross_section matrix) (0 -- (len-1) |> List.of_enum)

let sublist start finish xs = 
  let finish = Pervasives.min finish (List.length xs) in
  let start = Pervasives.min start finish in
  Array.sub (Array.of_list xs) start (finish-start) |> Array.to_list

let rotate_left i vector = sublist i (List.length vector) vector @ sublist 0 i vector
let rotate_right i = List.rev |- rotate_left i |- List.rev

let foldi func seed count = List.fold_left func seed (0 -- (count-1) |> List.of_enum)
let mapi func vector = List.map2 func (0 -- ((List.length vector)-1) |> List.of_enum) vector

let tap f x = (f x); x
