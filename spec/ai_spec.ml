#use "topfind"

#require "unix"
#require "ospecl"
open Ospecl.Spec
open Ospecl.Matchers

#require "batteries"
open Batteries_uni

#load "col.cmo"
#load "row.cmo"
#load "piece.cmo"
#load "line.cmo"
#load "lines.cmo"
#load "columns.cmo"
#load "board.cmo"
#load "player.cmo"
#load "game.cmo"
#load "ai.cmo"

let specs = [
  describe "AI" [
    describe ".minimax" begin
      let equal_to_float = within 0.0001 in [
        
        it "returns 0 if Board.empty" begin
          let module TestAI = Ai.Make(Board) in

          Board.empty |> TestAI.minimax 0 Piece.Yellow Piece.Yellow TestAI.winning_score =~ is equal_to_float 0.
        end;
        it "returns losing score if opponent has won" begin
          let module TestAI = Ai.Make (struct include Board
            let has_won player board =
              player = Piece.Red
          end) in

          TestAI.minimax 0 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is equal_to_float TestAI.losing_score
        end;
        it "with depth 0 returns value from eval function" begin
          let module TestAI = Ai.Make (struct include Board
            let evaluate _ _ = 5.
          end) in

          TestAI.minimax 0 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is equal_to_float 5.
        end;
        it "with depth 1 returns winning score if player can win this turn" begin
          let module TestAI = Ai.Make (struct include Board
            let has_won player board =
              player = Piece.Yellow
          end) in

          TestAI.minimax 1 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is equal_to_float TestAI.winning_score
        end;
        it "with depth 1 returns highest values from eval function after this turn" begin
          let module TestAI = Ai.Make (struct include Board
            let last_drop = ref None 
            let drop _ col board = 
              last_drop := Some col;
              board
            let evaluate player board =
              match !last_drop with
              | Some Col.Col2 -> 5.
              | _             -> 0.
          end) in

          TestAI.minimax 1 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is equal_to_float 5.
        end;
        it "with depth 2 returns highest of lowest eval values after 2 turns" begin
          let module TestAI = Ai.Make (struct include Board
            let yellow_col = ref None
            let red_col = ref None

            let drop piece col board = 
              begin 
                match piece with 
                | Piece.Yellow -> yellow_col := Some (col |> Col.to_int |> succ)
                | Piece.Red -> red_col := Some (col |> Col.to_int |> succ)
              end;
              board

            let evaluate player board =
              match (!yellow_col, !red_col) with
              | (Some y, Some r) -> y + r |> ( * ) (-1) |> float_of_int
              | _ -> failwith "should have dropped both a yellow and a red piece"
          end) in

          TestAI.minimax 2 Piece.Yellow Piece.Yellow TestAI.winning_score Board.empty =~ is equal_to_float (-8.)
        end;
        it "with depth 1 returns an full score value for a full column" begin
          let module TestAI = Ai.Make (struct include Board
            let drop _ col _ =
              raise (Board.Column_full col)
          end) in

          TestAI.minimax 1 Piece.Yellow Piece.Yellow TestAI.column_full_score Board.empty =~ is equal_to_float TestAI.column_full_score
        end;
      ] 
      end;
      describe ".choose_column" [
        it "returns the move with the highest score" begin
          let equal_to_col = equal_to Col.to_string in

          let module Board = struct include Board
            let last_drop = ref None

            let drop _ col board =
              last_drop := Some col;
              board

            let evaluate _ board =
              match !last_drop with
              | Some Col.Col4 -> 1.
              | _ -> 0.
          end 
          in
          let module TestAI = Ai.Make (Board) in

          TestAI.choose_column 0 Board.empty Piece.Yellow =~ is equal_to_col Col.Col4
        end;
      ];
    ];
]
