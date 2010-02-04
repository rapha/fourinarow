module Make (Board : sig
  type t = Board.t
  val wins : Player.t -> t -> bool
  val drop : Player.t -> int -> t -> t
  val evaluate : Player.t -> t -> float
end) = struct

  let rec minimax depth mover (player, opponent) board =
    let worst = if player = mover then min else max in
    let fail = worst infinity neg_infinity in
    if board |> Board.wins opponent then
       fail |> Enum.repeat ~times:7
    else
      let score =
        if depth <= 0 then
          Board.evaluate player
        else
          minimax (depth-1) mover (opponent,player) |- reduce worst
      in
      (1 -- 7) |> map (fun column ->
        try
          Board.drop player column board |> score
        with
          Failure "column full" -> fail
        )

  let choose_column depth game =
    let (mover, opponent) as players = game |> Game.Normal.players in
    let max_index =
      Enum.foldi (fun index value (maxi,max) -> if value >= max then (index,value) else (maxi,max)) (-1, neg_infinity)
      |- fst
    in
    game
    |> Game.Normal.board
    |> minimax depth mover players
    |> max_index
    |> (+) 1
end
