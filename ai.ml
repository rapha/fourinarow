module Make (Board : sig
  type t = Board.t
  val wins : Player.t -> t -> bool
  val drop : Player.t -> int -> t -> t
  val evaluate : Player.t -> t -> float
end) = struct

  let rec minimax depth mover (player, opponent) board =
    let best, worst = if player <> mover then (max, min) else (min, max) in
    let fail = best infinity neg_infinity in
    if board |> Board.wins opponent then
       fail |> Enum.repeat ~times:7
    else
      (1 -- 7)
      |> map (fun column ->
        try
          let score =
            if depth <= 0 then
              Board.evaluate player
            else
              minimax (depth-1) mover (opponent,player) |- reduce best
          in
          Board.drop player column board |> score
        with
          Failure "column full" -> fail
        )


  let choose_move depth mover game =
    game
    |> Game.Normal.board
    |> minimax depth mover (Game.Normal.players game)
    |> Enum.foldi (fun index value (maxi,max) ->
        if value >= max then (index,value) else (maxi,max)
        ) (-1, neg_infinity)
    |> fst
    |> (+) 1
end
