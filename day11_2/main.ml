include Aoc.Pvector

module Pvector = Aoc.Pvector
let (-->) = Pvector.(-->)

let row_len input = String.index input '\n' + 1
let pt i rl = i / rl, i mod rl
let piecewise_dist (x, y) (x2, y2) = Int.abs (y2 - y) + Int.abs (x2 - x)

let cumsum_empty_rows s = 
  let rows = String.fold_left (fun count c -> if c = '\n' then count + 1 else count) 1 s in
  if rows = 0 then Pvector.make_vec 0 0 else
  let (_, empty, cumsummed) = String.fold_left
    (fun (row_i, empty, grid) c -> 
      if c = '\n' then 
        let prev = if row_i = 0 then 0 else (grid --> (row_i - 1)) in 
        let addand = if empty then 1 else 0 in 
        (row_i + 1, true, Pvector.append (prev + addand) grid)
      else
        (row_i, empty && c = '.', grid) 
    ) (0, true, Pvector.empty) s
  in
  let addand = if empty then 1 else 0 in
  let prev = if rows > 1 then cumsummed --> (rows - 2) else 0 in
  Pvector.append (addand + prev) cumsummed

let cumsum_empty_cols s = 
  let rows = String.fold_left (fun count c -> if c = '\n' then count + 1 else count) 1 s in
  let cols = String.index s '\n' in
  let rec h empty grid rowi coli = 
    if coli = cols then grid else (* at the end, done *)
    if rowi = rows then 
      let prev = if coli = 0 then 0 else grid --> (coli - 1) in 
      let addand = if empty then 1 else 0 in 
      (* store result, compute next column *)
      h true (Pvector.append (prev + addand) grid) 0 (coli + 1)
    else
      (* still checking this column *)
      let c = String.get s (rowi * (cols + 1) + coli) in 
      h (empty && c = '.') grid (rowi + 1) coli
  in
  h true Pvector.empty 0 0

let extract_points s = 
  let l = String.length s in 
  let rl = row_len s in
  let rec h i acc = 
    if i = l then acc else
    let c = String.get s i in 
    match c with 
    | '#' -> h (i + 1) (Pvector.append (pt i rl) acc)
    | _ -> h (i + 1) acc
  in
  h 0 Pvector.empty

let get_pt_pairs vec = 
  let l = Pvector.len vec in 
  let rec h i1 i2 acc = 
    if i2 = l then acc else
    if i1 = i2 then 
      h 0 (i2 + 1) acc
    else 
      h (i1 + 1) i2 (Pvector.append (vec --> i1, vec --> i2) acc)
  in 
  h 0 0 Pvector.empty

let dist_with_mults ((a1, a2), (b1, b2)) cs_rows cs_cols factor = 
  let add_rows = (cs_rows --> max a1 b1) - (cs_rows --> min a1 b1) in 
  let add_cols = (cs_cols --> max a2 b2) - (cs_cols --> min a2 b2) in 
  piecewise_dist (a1, a2) (b1, b2) + add_rows * factor + add_cols * factor

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
  let cs_rows = cumsum_empty_rows input in 
  let cs_cols = cumsum_empty_cols input in 
  let pts = extract_points input in
  let pairs = get_pt_pairs pts in 
  let res = 
    Pvector.fold_left 
    (fun acc pair -> acc + dist_with_mults pair cs_rows cs_cols 999999) 
    0
    pairs
  in 
  Printf.printf "%d\n" res