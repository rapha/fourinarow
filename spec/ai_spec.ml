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
    describe ".choose_column" begin
      let equal_to_col = equal_to Col.to_string in [
      it "avoids columns which are full" begin
        let module TestMinimax = Ai.Make(struct include Board
          let drop _ col board =
            if col != Col.Col3 then raise (Column_full col) else
            board
        end)

        in
        TestMinimax.choose_column 1 Board.empty Piece.Yellow =~ is equal_to_col Col.Col3
      end;
      describe "with depth of 1" begin
        let depth = 1 in [
        describe "where the mover can win with a single drop in some column" [
          it "chooses that column" begin
            let module TestMinimax = Ai.Make(struct include Board
              let wins = ref false
              let drop piece col board =
                wins := (col = Col.Col4 && piece = Piece.Yellow);
                board

              let has_won piece board = !wins
            end)
            in

            TestMinimax.choose_column depth Board.empty Piece.Yellow =~ is equal_to_col Col.Col4
          end
        ];
        describe "where the mover cannot win with one drop" [
          it "chooses the column with the highest evaluation" begin
            let module TestMinimax = Ai.Make(struct include Board
              let value = ref 0.
              let drop piece col board =
                value := 
                  if col = Col.Col4 && piece = Piece.Yellow then 
                    1. 
                  else 
                    0.;
                board
              let evaluate _ _ = !value
            end)
            in

            TestMinimax.choose_column depth Board.empty Piece.Yellow =~ is equal_to_col Col.Col4
          end
        ]
      ] 
      end;
      describe "with depth of 2" begin
        let depth = 2 in [
        describe "where the opponent can win on their next move if mover drops in some column" [
          it "does not choose that column" begin
            let yellow_and_red_in_all_columns_but_3 = 
              Col.left_to_right
              |> List.filter ((!=) Col.Col3)
              |> List.map (fun col -> Board.empty |> Board.drop Piece.Yellow col |> Board.drop Piece.Red col)
            in

            let module TestMinimax = Ai.Make(struct include Board
              let has_won piece board =
                piece = Piece.Red && List.mem board yellow_and_red_in_all_columns_but_3
            end)

            in 
            TestMinimax.choose_column depth Board.empty Piece.Yellow =~ is equal_to_col Col.Col3
          end
        ];
        describe "where the opponent cannot win on their next move" [
          it "chooses the column with the least low evaluation" begin
            let module TestMinimax = Ai.Make (struct include Board
              let yellow_col = ref None
              let red_col = ref None

              let drop piece col board = 
                begin 
                  match piece with 
                  | Piece.Yellow -> yellow_col := Some (col |> Col.to_int)
                  | Piece.Red -> red_col := Some (col |> Col.to_int)
                end;
                board

              (* the further right they drop, the lower the score after 2 turns *)
              let evaluate player board =
                match (!yellow_col, !red_col) with
                | (Some y, Some r) -> y + r |> ( * ) (-1) |> float_of_int
                | _ -> failwith "should have dropped both a yellow and a red piece"
            end) 
            in

            TestMinimax.choose_column depth Board.empty Piece.Yellow =~ is equal_to_col Col.Col1
          end
        ]
      ]
      end;
    ] 
    end;
  ];
]
