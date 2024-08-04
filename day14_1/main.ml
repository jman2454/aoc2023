(* now need to track how many dists I can add to myself (in my column) *)
(* that number is equal to however many slots the guy above me will slide + 1 - (number of Os above me) *)

open Aoc.Public

let get row col v = 
  v --> row --> col

let set row col value v = 
  (v, row) <-- ((v --> row, col) <-- value)

let ezpz s = 
  let slen = String.length s in 
  let nl_ind = String.index s '\n' in 
  let lines = (slen + 1) / (nl_ind + 1) in

  let row_len = nl_ind + 1 in 
  let grid = Pvector.make_vec lines (Pvector.make_vec nl_ind 0) in
  let rec h i dist acc grid = 
    if i >= slen then acc else
    let c = String.get s i in 
    let row, col = i / row_len, i mod row_len in 
    if c = '\n' then h (i + 1) (dist - 1) acc grid else 
    if c = 'O' then 
      let grid = (set row col (if row = 0 then 0 else get (row - 1) col grid) grid) in
      h (i + 1) dist (acc + dist + get row col grid) grid
    else if c = '.' then 
      h (i + 1) dist acc (set row col (1 + if row = 0 then 0 else get (row - 1) col grid) grid)
    else
      h (i + 1) dist acc (set row col 0 grid)
  in
  h 0 lines 0 grid

let () = ezpz "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...." |> Printf.printf "%d\n";