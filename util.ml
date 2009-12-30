let tap f x = (f x); x

let transpose len matrix =
  let cross_section i = matrix |> List.map (List.drop i |- List.enum |- Enum.peek |- Option.default None)
  in List.map cross_section (0 -- (len-1) |> List.of_enum)

let sublist start finish = 
  List.drop start |- List.take (finish-start)

let rotate_left i vector = 
  sublist i (List.length vector) vector @ sublist 0 i vector

let rotate_right i = 
  List.rev |- rotate_left i |- List.rev
