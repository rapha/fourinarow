type 'a spot = ('a * bool) (* (cell, filled) *)

exception Line_full

type 'a t = {first: 'a spot; second: 'a spot; third: 'a spot; fourth: 'a spot}

let create cell_a cell_b cell_c cell_d = {
  first = (cell_a, false);
  second = (cell_b, false);
  third = (cell_c, false);
  fourth = (cell_d, false);
}

let fill cell line =
  match line with
  | {first = (c, false)} when c = cell -> { line with first = (c, true) }
  | {second = (c, false)} when c = cell -> { line with second = (c, true) }
  | {third = (c, false)} when c = cell -> { line with third = (c, true) }
  | {fourth = (c, false)} when c = cell -> { line with fourth = (c, true) }
  | _ -> line

let is_full = function
  | {first = (_, true); second = (_, true); third = (_, true); fourth = (_, true)} -> 
      true
  | _ -> 
      false

let includes cell line =
  match line with
  | {first = (c, _)} when c = cell -> true
  | {second = (c, _)} when c = cell -> true
  | {third = (c, _)} when c = cell -> true
  | {fourth = (c, _)} when c = cell -> true
  | _ ->
      false
