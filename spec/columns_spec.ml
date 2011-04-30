#use "topfind"

#require "unix"
#require "ospecl"
open Ospecl.Spec
open Ospecl.Matchers

#require "batteries"
open Batteries_uni

#load "col.cmo"
#load "row.cmo"
#load "columns.cmo"

let specs = [
  describe "Columns" [
    describe ".append" begin
      let equals_row = equal_to Row.to_string in 
      let open Col in [
      describe "on Columns.empty" [
        it "returns Row1 in any row" begin
          let rows = 
            left_to_right 
            |> List.map (fun col -> Columns.empty |> Columns.append col "something" |> snd) 
          in

          rows =~ every_item (equals_row Row.Row1)
        end
      ];
      it "returns increasing rows in the same column" begin
        let open Row in

        let equals_row_list = equal_to_list Row.to_string in

        let actual_rows =
          [Col1; Col1; Col1] 
          |> List.fold_left (fun (cols, rows) col ->
              let (cols, row) = Columns.append col "a" cols in
              (cols, row::rows)
          ) (Columns.empty, []) 
          |> snd |> List.rev

        in
        actual_rows =~ equals_row_list [Row1; Row2; Row3]
      end;
      it "throws Column_full after 6 drops in a given column" begin
        let cols = 
          List.fold_left 
            (fun cols col -> Columns.append col "a" cols |> fst) 
            Columns.empty 
            [Col1; Col1; Col1; Col1; Col1; Col1] 
        in

        (fun _ -> Columns.append Col1 "a" cols) =~ does raise_exn Columns.Full
      end
    ]
    end;
  ];
]
