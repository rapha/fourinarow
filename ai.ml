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

  let rec minimax depth mover ((current_piece, opponent_piece) as pieces) limit_score board =
    let best, worst = if current_piece = mover then (max,min) else (min,max) in
    let fail = worst winning_score losing_score in
    if Board.wins opponent_piece board then
      fail /. (depth + 1 |> float_of_int) (* winning sooner has a higher weight than winning later *)
    else
      if depth <= 0 then
        Board.evaluate mover board
      else
        let child_score_with_limit = child_score (depth-1) board mover pieces in
        let rec best_child_score champion = function
          | [] -> champion
          | column :: rest ->
              let contender =
                column |> child_score_with_limit champion
              in
              if (best contender limit_score) = contender then
                contender
              else
                best_child_score (best champion contender) rest
        in
        best_child_score fail (0 -- 6 |> List.of_enum)

  and child_score depth board mover (current_piece, opponent_piece) limit_score column =
    try
      board |> Board.drop current_piece column |> minimax depth mover (opponent_piece, current_piece) limit_score
    with
      Board.Column_full _ ->
        if current_piece = mover then column_full_score else (column_full_score *. -1.)


  let choose_column depth board ((mover, opponent_piece) as pieces) =
    let max_index =
      Enum.foldi (fun index value (maxi,max) ->
        if value >= max then (index,value) else (maxi,max)) (-1, column_full_score)
      |- fst
    in
    (0 -- 6)
    |> map (child_score depth board mover pieces column_full_score)
    |> max_index
end
