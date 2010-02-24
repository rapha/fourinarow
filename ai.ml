module Make (Board : sig
  type t = Board.t
  val wins : Player.t -> t -> bool
  val drop : Player.t -> int -> t -> t
  val evaluate : Player.t -> t -> float
end) = struct

  let losing_score = -100.
  let winning_score = losing_score *. -1.
  let full_score = neg_infinity

  let rec minimax depth mover ((player, opponent) as players) limit_score board =
    let best, worst = if player = mover then (max,min) else (min,max) in
    let fail = worst winning_score losing_score in
    if Board.wins opponent board then
      fail /. (depth + 1 |> float_of_int) (* winning sooner is worse than winning later *)
    else
      if depth <= 0 then
        Board.evaluate player board
      else
        let child_score_with_limit = child_score (depth-1) board mover players (worst full_score (full_score *. -1.)) in
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

  and child_score depth board mover (player, opponent) full limit_score column =
    try
      board |> Board.drop player column |> minimax depth mover (opponent, player) limit_score
    with
      Failure "column full" -> full


  let choose_column depth game =
    let (mover, opponent) as players = game |> Game.Normal.players in
    let max_index =
      Enum.foldi (fun index value (maxi,max) ->
        if value >= max then (index,value) else (maxi,max)) (-1, losing_score)
      |- fst
    in
    let board = game |> Game.Normal.board in
    (0 -- 6)
    |> map (child_score depth board mover players full_score losing_score)
    |> max_index
end
