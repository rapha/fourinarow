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

let specs = [
  describe "Board" begin 
    let open Col in 
    let equal_to_board = equal_to (fun _ -> "<board>") in
    [
      describe ".drop" [
        describe "in an Board.empty column" [
          it "puts a piece on the first row of that column" begin
            let expected = Board.of_string (
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "-------\n" ^
              "Y------\n" 
            ) 
            in
            Board.empty |> Board.drop Piece.Yellow Col1 =~ is equal_to_board expected
          end;
        ];
        describe "in a full column" [
          it "raises Column_full" begin
            let board = Board.of_string (
              "R------\n" ^
              "Y------\n" ^
              "R------\n" ^
              "Y------\n" ^
              "R------\n" ^
              "Y------\n" 
            ) 
            in
            (fun () -> board |> Board.drop Piece.Yellow Col1) =~ does raise_exn (Board.Column_full Col1)
          end;
        ];
      ];

      describe ".has_won" [
        describe "on an Board.empty board" [
          it "is false for both players" begin
            ignore (Board.empty |> Board.has_won Piece.Yellow =~ is false');
            Board.empty |> Board.has_won Piece.Red =~ is false'
          end
        ];
        it "is false for vertical line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "Y------\n" ^
            "Y------\n" ^
            "Y------\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is false'
        end;
        it "is true for vertical line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "Y------\n" ^
            "Y------\n" ^
            "Y------\n" ^
            "Y------\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is true'
        end;
        it "is false for horizontal line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "YYY----\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is false'
        end;
        it "is true for horizontal line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "YYYY---\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is true'
        end;
        it "is false for NE line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "--Y----\n" ^
            "-YR----\n" ^
            "YRR----\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is false'
        end;
        it "is true for NE line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "---Y---\n" ^
            "--YR---\n" ^
            "-YRR---\n" ^
            "YRRR---\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is true'
        end;
        it "is false for NW line of 3" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "-------\n" ^
            "Y------\n" ^
            "RY-----\n" ^
            "RRY----\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is false'
        end;
        it "is true for NW line of 4" begin
          let board = Board.of_string (
            "-------\n" ^
            "-------\n" ^
            "Y------\n" ^
            "RY-----\n" ^
            "RRY----\n" ^
            "RRRY---\n")
          in 
          board |> Board.has_won Piece.Yellow =~ is true'
        end;
        it "is false when there is a gap in the line" begin
          let board = Board.of_string (
            "-------\n" ^
            "----R--\n" ^
            "---RR--\n" ^
            "--RYY--\n" ^
            "--YYY--\n" ^
            "R-YYY--\n")
          in 
          board |> Board.has_won Piece.Red =~ is false'
        end;
      ];
    ] 
  end;
]
