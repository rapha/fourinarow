open Batteries_uni

module Make (Board : sig
  type t = Board.t
  exception Column_full of Col.t
  val has_won : Piece.t -> t -> bool
  val drop : Piece.t -> Col.t -> t -> t
  val evaluate : Piece.t -> t -> float
end) : sig
  val choose_column : int -> Board.t -> Piece.t -> Col.t
end = struct

  let losing_score = -100.
  let winning_score = losing_score *. -1.
  let column_full_score = neg_infinity

  let rec score_of depth mover next_piece limit_score board =
    let best_of, worst_of = if next_piece = mover then (max,min) else (min,max) in
    let fail = worst_of winning_score losing_score in

    let prev_piece = Piece.opponent next_piece in
    if board |> Board.has_won prev_piece then
      fail /. (depth + 1 |> float_of_int) (* winning sooner has a higher weight than winning later *)
    else
      if depth <= 0 then
        board |> Board.evaluate mover
      else
        let rec best_score_of_child best_so_far = function
          | [] -> best_so_far
          | column :: rest ->
              let score_of_this_child = column |> score_of_child depth board mover next_piece best_so_far in
              if (best_of score_of_this_child limit_score) = score_of_this_child then
                score_of_this_child
              else
                best_score_of_child (best_of best_so_far score_of_this_child) rest
        in
        Col.left_to_right |> best_score_of_child fail

  and score_of_child depth board mover next_piece limit_score column =
    try
      board |> Board.drop next_piece column |> score_of (depth-1) mover (Piece.opponent next_piece) limit_score
    with
      Board.Column_full _ ->
        if next_piece = mover then column_full_score else (column_full_score *. -1.)

  let choose_column depth board mover =
    let max_index =
      Enum.foldi (fun index value (maxi,max) ->
        if value >= max then (index,value) else (maxi,max)) (-1, column_full_score)
      |- fst
    in
    Col.left_to_right |> List.enum
    |> map (score_of_child depth board mover mover column_full_score)
    |> max_index |> Col.of_int
end
