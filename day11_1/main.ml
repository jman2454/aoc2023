include Aoc.Pvector

module Pvector = Aoc.Pvector
let (<--) = Pvector.(<--)
let (-->) = Pvector.(-->)

(* let row_len input = String.index input '\n' + 1 *)
(* let pt i rl = i mod rl, i / rl *)
(* let piecewise_dist (x, y) (x2, y2) = Int.abs (y2 - y) + Int.abs (x2 - x) *)

let cumsum_empty_rows s = 
  let rows = String.fold_left (fun count c -> if c = '\n' then count + 1 else count) 1 s in
  if rows = 0 then Pvector.make_vec 0 0 else
  let (_, empty, cumsummed) = String.fold_left
    (fun (row_i, empty, grid) c -> 
      if c = '\n' then 
        let prev = if row_i = 0 then 0 else (grid --> (row_i - 1)) in 
        let addand = if empty then 1 else 0 in 
        (row_i + 1, true, (grid, row_i) <-- prev + addand)
      else
        (row_i, empty && c = '.', grid) 
    ) (0, true, Pvector.make_vec rows 0) s
  in
  let addand = if empty then 1 else 0 in
  let prev = if rows > 1 then cumsummed --> (rows - 2) else 0 in
  (cumsummed, rows - 1) <-- addand + prev

let cumsum_empty_cols s = 
  let rows = String.fold_left (fun count c -> if c = '\n' then count + 1 else count) 1 s in
  let cols = String.index s '\n' in
  let rec h empty grid rowi coli = 
    if coli = cols then grid else (* at the end, done *)
    if rowi = rows then 
      let prev = if coli = 0 then 0 else grid --> (coli - 1) in 
      let addand = if empty then 1 else 0 in 
      (* store result, compute next column *)
      h true ((grid, coli) <-- (prev + addand)) 0 (coli + 1)
    else
      (* still checking this column *)
      let c = String.get s (rowi * (cols + 1) + coli) in 
      h (empty && c = '.') grid (rowi + 1) coli
  in
  h true (Pvector.make_vec cols 0) 0 0

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

let () =
cumsum_empty_rows input |> Pvector.to_str string_of_int |> Printf.printf "Rows cumsummed: %s\n";
cumsum_empty_cols input |> Pvector.to_str string_of_int |> Printf.printf "Cols cumsummed: %s\n";