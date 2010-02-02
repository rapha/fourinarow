module Make (Board : sig
  type t = Board.t
  val wins : Player.t -> t -> bool
  val drop : Player.t -> int -> t -> t
  val evaluate : Player.t -> t -> float
end) = struct

  let rec minimax depth mover (player, opponent) board =
    let best, worst = if player <> mover then (max, min) else (min, max) in
    if board |> Board.wins opponent then
      best infinity neg_infinity |> Enum.repeat ~times:7
    else
      let scores =
        if depth <= 0 then
          map (Board.evaluate player)
        else
          map (minimax (depth-1) mover (opponent,player)) |- map (reduce best)
      in
      (1 -- 7) |> map (fun column -> Board.drop player column board) |> scores

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
