#use "topfind"

#require "unix"
#require "ospecl"
open Ospecl.Spec
open Ospecl.Matchers

#load "piece.cmo"

let specs = [
  describe "Piece.to_char and Piece.of_char are inverse" [
      it "piece -> char -> piece" begin
        let equal_to_piece = equal_to Piece.to_string in

        Piece.of_char (Piece.to_char Piece.Yellow) =~ is equal_to_piece Piece.Yellow
      end;
      it "char -> piece -> char" begin
        Piece.to_char (Piece.of_char 'Y') =~ is equal_to_char 'Y'
      end;
  ];
]
