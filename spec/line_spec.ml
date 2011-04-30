#use "topfind"

#require "unix"
#require "ospecl"
open Ospecl.Spec
open Ospecl.Matchers

#require "batteries"
open Batteries_uni

#load "line.cmo"

let specs = [
  describe "Line" [
    describe ".include cell" [
      it "returns true if the line was created with that cell" begin
        Line.create "a" "b" "c" "d" |> Line.includes "a" =~ is true'
      end;
      it "returns false if the line was not created with that cell" begin
        Line.create "a" "b" "c" "d" |> Line.includes "z" =~ is false'
      end
    ];
    describe ".all_filled" [
      describe "when Line.fill has been called for each cell" [
        it "returns true" begin
          let line = Line.create "a" "b" "c" "d" in
          let line = ["a"; "b"; "c"; "d"] |> List.fold_left (fun line cell -> Line.fill cell line) line
          in 
          Line.all_filled line =~ is true'
        end
      ];
      describe "when Line.fill has not been called for each cell" [
        it "returns true" begin
          let line = Line.create "a" "b" "c" "d" in
          let line = ["a"; "b"; "c"] |> List.fold_left (fun line cell -> Line.fill cell line) line
          in 
          Line.all_filled line =~ is false'
        end
      ];
      it "is not affected by calling fill with cells that are not included" begin
        let line = Line.create "a" "b" "c" "d" in
        let line = ["w"; "x"; "y"; "z"] |> List.fold_left (fun line cell -> Line.fill cell line) line
        in 
        Line.all_filled line =~ is false'
      end
    ];
  ]
]
