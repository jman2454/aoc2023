include Aoc.Pvector

let row_len input = String.index input '\n' + 1
let pt i rl = i mod rl, i / rl
let piecewise_dist (x, y) (x2, y2) = Int.abs (y2 - y) + Int.abs (x2 - x)


let from i init = 
  let rec h i l = 
    if i = 0 then 
      l
    else
      h (i - 1) (init::l) in 
  h i []

let split str pos = 
  let (l, r, _) = String.fold_left (
    fun (l, r, i) c -> 
      if i <= pos then 
        (l ^ String.make 1 c, r, i + 1) 
      else 
        (l, r ^ String.make 1 c, i + 1)
  ) ("", "", 0) str in 
  (l, r)

let rec rewrite_line line cols sub acc =   
  match cols with 
  | next::rest -> 
    let (l, r) = split line (next - sub) in
      rewrite_line r rest (next + 1) (acc ^ l ^ ".")
  | [] -> acc ^ line

let rec rewrite extra_count lines rows cols result i = 
  match lines with 
  | line::rest -> 
    (match rows with 
    | next::r_rows -> 
      let n_result = (result ^ rewrite_line line cols 0 "" ^ "\n") in
      let (f_result, n_rows) = if i = next then (n_result ^ (String.make extra_count '.' ^ "\n"), r_rows) else (n_result, rows) in
      rewrite extra_count rest n_rows cols f_result (i + 1)
    | [] -> 
      result ^ String.concat "\n" (lines |> List.map (fun l -> rewrite_line l cols 0 "")))
  | [] -> result

let inds bools = List.mapi (fun i b -> if b then Some(i) else None) bools |> List.filter_map (fun op -> op)

let blowup input = 
  let rl = row_len input in 
  let lines = String.split_on_char '\n' input in
  (* ignore newlines? *)
  let (rows, cols) = 
    List.fold_right (
      fun line (rows, cols) -> 
        (if String.for_all ((=) '.') line then true::rows else false::rows), List.mapi (fun i b -> b && line.[i] <> '#') cols
      )
      lines
      ([], from (rl - 1) true)
  in
  let (rows, cols) = (inds rows, inds cols) in 
  rewrite (rl + List.length cols - 1) lines rows cols "" 0
  
let ans input =
  let rl = row_len input in 
  let il = String.length input in 
  let rec h i sum lst = 
    if i = il then 
      sum
    else if input.[i] = '#' then 
      let p = pt i rl in
      let new_sum = List.fold_left (+) sum @@ List.map (fun other -> piecewise_dist (pt other rl) p) lst in 
      h (i + 1) new_sum (i::lst)
    else
      h (i + 1) sum lst in
  h 0 0 []

let input = "...#......
.......#..
#.........
..........
......#...
.#........
.........#
..........
.......#..
#...#....."

(* store counts of points "behind" - then add millions to sum *)

(* print orders - should min order be 4? it might *)

let print_slots_transparent vec = 
  Aoc.Pvector.count_slots vec |> Printf.printf "%d\n";
  vec

let print_tree_transparent vec = 
  Aoc.Pvector.tree_to_str vec |> Printf.printf "%s\n";
  vec

let () =
input |> blowup |> ans |> Printf.printf "%d\n";
let t = Aoc.Pvector.make_vec 1 0 |> print_slots_transparent |> print_tree_transparent
(* |> Aoc.Pvector.append 1 |> print_slots_transparent |> print_tree_transparent
|> Aoc.Pvector.append 2 |> print_slots_transparent |> print_tree_transparent
|> Aoc.Pvector.append 3 |> print_slots_transparent |> print_tree_transparent
|> Aoc.Pvector.append 4 |> print_slots_transparent |> print_tree_transparent
|> Aoc.Pvector.append 5 |> print_slots_transparent |> print_tree_transparent *)
in 
Printf.printf "getting values";
t |> Aoc.Pvector.at 0 |> Printf.printf "%d\n";
(* t |> Aoc.Pvector.at 1 |> Printf.printf "%d\n";
t |> Aoc.Pvector.at 2 |> Printf.printf "%d\n";
t |> Aoc.Pvector.at 3 |> Printf.printf "%d\n";
t |> Aoc.Pvector.at 4 |> Printf.printf "%d\n";
t |> Aoc.Pvector.at 5 |> Printf.printf "%d\n"; *)
(* t |> Aoc.Pvector.count_slots |> Printf.printf "%d\n";
t |> Aoc.Pvector.tree_to_str |> Printf.printf "%s\n" *)