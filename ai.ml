module Make (Board : sig
  type t = Board.t
  val wins : Player.t -> t -> bool
  val drop : Player.t -> int -> t -> t
  val evaluate : Player.t -> t -> float
end) = struct

  let win, lose = 100., (-100.)

  let rec minimax depth mover (player, opponent) limit board =
    let best, worst = if player = mover then (max,min) else (min,max) in
    let fail = worst win lose in
    if Board.wins opponent board then
      fail /. (depth + 1 |> float_of_int) (* winning sooner is worse than winning later *)
    else
      if depth <= 0 then
        Board.evaluate player board
      else
        let rec best_child_score champion = function
          | [] -> champion
          | column :: rest ->
              let contender =
                try board |> Board.drop player column |> minimax (depth-1) mover (opponent,player) champion
                with Failure "column full" -> fail
              in
              if (best contender limit) = contender then
                contender
              else
                best_child_score (best champion contender) rest
        in
        best_child_score fail (1 -- 7 |> List.of_enum)

  let choose_column depth game =
    let (mover, opponent) = game |> Game.Normal.players in
    let max_index =
      Enum.foldi (fun index value (maxi,max) ->
        if value >= max then (index,value) else (maxi,max)) (-1, lose)
      |- fst
    in
    let board = game |> Game.Normal.board in
    (1 -- 7)
    |> map (fun column -> Board.drop mover column board)
    |> map (minimax depth mover (opponent, mover) lose)
    |> max_index
    |> (+) 1
end
