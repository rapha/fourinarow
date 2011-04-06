type 'a point = Filled of 'a | Empty of 'a

exception Line_full

type 'a t = {first: 'a point; second: 'a point; third: 'a point; fourth: 'a point}

let create cell_a cell_b cell_c cell_d = {
  first = Empty cell_a;
  second = Empty cell_b;
  third = Empty cell_c;
  fourth = Empty cell_d;
}

let fill cell line =
  match line with
  | {first = Empty c} when c = cell -> { line with first = Filled c }
  | {second = Empty c} when c = cell -> { line with second = Filled c }
  | {third = Empty c} when c = cell -> { line with third = Filled c }
  | {fourth = Empty c} when c = cell -> { line with fourth = Filled c }
  | _ -> line

let is_full = function
  | {first = Filled _; second = Filled _; third = Filled _; fourth = Filled _} -> 
      true
  | _ -> 
      false

let includes cell line =
  match line with
  | {first = Empty c} when c = cell -> true
  | {first = Filled c} when c = cell -> true
  | {second = Empty c} when c = cell -> true
  | {second = Filled c} when c = cell -> true
  | {third = Empty c} when c = cell -> true
  | {third = Filled c} when c = cell -> true
  | {fourth = Empty c} when c = cell -> true
  | {fourth = Filled c} when c = cell -> true
  | _ ->
      false
