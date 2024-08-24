open Aoc.Public

let set (row, col) value v = 
  (v, row) <-- ((v --> row, col) <-- value)

let get_opt (row, col) v = 
  if row < 0 || col < 0 then None else
  if row < Pvector.len v && col < (Pvector.len (v --> row)) then 
    Some((v --> row) --> col)
  else
    None

let doi_n fn cap arg = 
  let rec h n acc = 
    if n = cap then acc else 
    h (n + 1) (fn n acc)
  in
  h 0 arg

let do_n fn cap arg = 
  doi_n (fun _ a -> fn a) cap arg

let update_roll acc = function 
| '.' -> acc + 1 
| '#' -> 0
| 'O' -> acc
| _ -> failwith "wow!"

(* coords are double-inverted; i.e. row index corresponds to y, and low row indices are high y values *)
let walk (row, col) amt = function 
| `Left -> (row, col - amt)
| `Right -> (row, col + amt)
| `Up -> (row - amt, col)
| `Down -> (row + amt, col)

let opposite = function | `Left -> `Right | `Right -> `Left | `Up -> `Down | `Down -> `Up 

let extremum grid = function 
| `Left -> 0 
| `Right -> Pvector.len (grid --> 0) - 1 
| `Up -> 0
| `Down -> Pvector.len grid - 1

let perpendicular = function | `Left -> `Up | `Right -> `Up | `Down -> `Right | `Up -> `Right

let roll dir grid = 
  let opp_dir = opposite dir in 
  let rec h pos (roll_amt, grid) = 
    match get_opt pos grid with 
    | None -> (roll_amt, grid)
    | Some c -> 
      h (walk pos 1 opp_dir) (update_roll roll_amt c, 
        match c with 
        | 'O' when roll_amt > 0 -> set (walk pos roll_amt dir) 'O' grid |> set pos '.'
        | 'O' -> grid
        | _ -> grid
      )
  in
  let start = 
    if dir = `Left || dir = `Right then 
      (extremum grid (perpendicular dir), extremum grid (dir))
    else 
      (extremum grid dir, extremum grid (perpendicular dir))
  in
  let rec h2 pos grid = 
    match get_opt pos grid with 
    | None -> grid
    | Some _ -> 
      let _, res = h pos (0, grid) in
      h2 (walk pos 1 (perpendicular dir |> opposite)) res
  in
  h2 start grid

(* if differently sized, unspecified behavior [or error] *)
let grids_equal g1 g2 = 
  Pvector.fold_lefti (fun acc row_i row -> 
    acc && Pvector.fold_lefti (fun acc col_i v1 -> acc && (g2 --> row_i --> col_i) = v1) true row
  ) true g1

let cycle grid = 
  roll `Up grid 
  |> roll `Left
  |> roll `Down
  |> roll `Right

(* let str_of_char_grid grid = Pvector.string_of_grid grid (String.make 1) *)

module CharPgrid = struct
  type t = char Pvector.t Pvector.t
  let equal = grids_equal

  (* ty claude for the hash fn *)
  let hash vec = 
    let rows = Pvector.len vec in
    let cols = if rows > 0 then Pvector.len (vec --> 0) else 0 in
    let prime1 = 31 in
    let prime2 = 37 in
    Pvector.fold_left (fun acc row ->
      Pvector.fold_left (fun inner_acc ch ->
        (inner_acc * prime1 + Char.code ch) land 0x3FFFFFFF
      ) acc row
    ) (rows * prime2 + cols) vec
end

module CharPgridHashTable = Hashtbl.Make(CharPgrid)

let max_cycles = 1000000000
let find_steady grid = 
  let tbl = CharPgridHashTable.create 0 in
  let rec h ct g = 
    if ct = max_cycles then 
      g
    else
      match CharPgridHashTable.find_opt tbl g with 
      | Some pos -> 
        let cycle_len = ct - pos in 
        let remaining_at_cycle_start = max_cycles - pos in 
        do_n cycle (remaining_at_cycle_start mod cycle_len) g
      | None -> 
        CharPgridHashTable.add tbl g ct;
        h (ct + 1) (cycle g)
  in
  h 0 grid

let load grid = 
  let mult_row_rocks factor = Pvector.fold_left (fun acc c -> acc + (if c = 'O' then factor else 0)) 0 in
  let total_rows = Pvector.len grid in 
  doi_n (fun row_i load -> load + mult_row_rocks (total_rows - row_i) (grid --> row_i)) total_rows 0

let char_grid_of_string s = 
  String.split_on_char '\n' s 
  |> Pvector.of_list 
  |> Pvector.map (fun s -> String.to_seq s |> List.of_seq |> Pvector.of_list) 

let ezpz s = 
  s 
  |> char_grid_of_string
  |> find_steady
  |> load

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