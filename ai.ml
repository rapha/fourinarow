module Make (Board : sig
  type t = Board.t
  exception Column_full of int
  val wins : Piece.t -> t -> bool
  val drop : Piece.t -> int -> t -> t
  val evaluate : Piece.t -> t -> float
end) = struct

  let losing_score = -100.
  let winning_score = losing_score *. -1.
  let column_full_score = neg_infinity

  let rec minimax depth mover current limit_score board =
    let best, worst = if current = mover then (max,min) else (min,max) in
    let fail = worst winning_score losing_score in
    if board |> Board.wins (Piece.opponent current) then
      fail /. (depth + 1 |> float_of_int) (* winning sooner has a higher weight than winning later *)
    else
      if depth <= 0 then
        board |> Board.evaluate mover
      else
        let child_score_with_limit = child_score (depth-1) board mover current in
        let rec best_child_score best_so_far = function
          | [] -> best_so_far
          | column :: rest ->
              let this_child_score = column |> child_score_with_limit best_so_far in
              if (best this_child_score limit_score) = this_child_score then
                this_child_score
              else
                best_child_score (best best_so_far this_child_score) rest
        in
        best_child_score fail (0 -- 6 |> List.of_enum)

  and child_score depth board mover current limit_score column =
    try
      board |> Board.drop current column |> minimax depth mover (Piece.opponent current) limit_score
    with
      Board.Column_full _ ->
        if current = mover then column_full_score else (column_full_score *. -1.)


  let choose_column depth board mover =
    let max_index =
      Enum.foldi (fun index value (maxi,max) ->
        if value >= max then (index,value) else (maxi,max)) (-1, column_full_score)
      |- fst
    in
    (0 -- 6)
    |> map (child_score depth board mover mover column_full_score)
    |> max_index
end
