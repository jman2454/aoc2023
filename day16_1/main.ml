open Aoc.Public

let get_opt (row, col) v = 
  if row < 0 || col < 0 then None else
  if row < Pvector.len v && col < (Pvector.len (v --> row)) then 
    Some((v --> row) --> col)
  else
    None

(* coords are double-inverted; i.e. row index corresponds to y, and low row indices are high y values *)
let walk (row, col) amt = function 
| `Left -> (row, col - amt)
| `Right -> (row, col + amt)
| `Up -> (row - amt, col)
| `Down -> (row + amt, col)

let set_dir (row, col) dir grid = 
let (l, r, u, d) = grid --> row --> col in 
let new_val = 
  match dir with
  | `Left -> (l + 1, r, u, d)
  | `Right -> (l, r + 1, u, d)
  | `Up -> (l, r, u + 1, d)
  | `Down -> (l, r, u, d + 1)
in 
let res = (grid, row) <-- ((grid --> row, col) <-- new_val) in 
res

let excitement (l,r,u,d) = if l+r+u+d > 0 then 1 else 0

let is_active (l,r,u,d) = function 
| `Left -> l > 0
| `Right -> r > 0
| `Up -> u > 0
| `Down -> d > 0

let dfs grid = 
  let result = Pvector.map (Pvector.map (fun _ -> (0,0,0,0))) grid in
  let rec step_beam pos dir acc = 
    let (row, col) = pos in 
    match get_opt pos grid with 
    | None -> acc
    | Some c -> 
      if is_active (acc --> row --> col) dir then acc else 
     ( match c with
      | '|' -> 
        (match dir with
        | `Left -> step_beam (walk pos 1 `Up) `Up (set_dir pos `Up acc |> set_dir pos `Down) |> step_beam (walk pos 1 `Down) `Down
        | `Right -> step_beam (walk pos 1 `Up) `Up (set_dir pos `Up acc |> set_dir pos `Down) |> step_beam (walk pos 1 `Down) `Down
        | `Up -> step_beam (walk pos 1 dir) dir (set_dir pos dir acc)
        | `Down -> step_beam (walk pos 1 dir) dir (set_dir pos dir acc))
      | '-' -> 
        (match dir with
        | `Left -> step_beam (walk pos 1 dir) dir (set_dir pos dir acc)
        | `Right -> step_beam (walk pos 1 dir) dir (set_dir pos dir acc)
        | `Up -> step_beam (walk pos 1 `Left) `Left (set_dir pos `Left acc |> set_dir pos `Right) |> step_beam (walk pos 1 `Right) `Right
        | `Down -> step_beam (walk pos 1 `Left) `Left (set_dir pos `Left acc |> set_dir pos `Right) |> step_beam (walk pos 1 `Right) `Right)
      | '/' -> 
        (match dir with
        | `Left -> step_beam (walk pos 1 `Down) `Down (set_dir pos dir acc)
        | `Right -> step_beam (walk pos 1 `Up) `Up (set_dir pos dir acc)
        | `Up -> step_beam (walk pos 1 `Right) `Right (set_dir pos dir acc)
        | `Down -> step_beam (walk pos 1 `Left) `Left (set_dir pos dir acc))
      | '\\' -> 
        (match dir with
        | `Left -> step_beam (walk pos 1 `Up) `Up (set_dir pos dir acc)
        | `Right -> step_beam (walk pos 1 `Down) `Down (set_dir pos dir acc)
        | `Up -> step_beam (walk pos 1 `Left) `Left (set_dir pos dir acc)
        | `Down -> step_beam (walk pos 1 `Right) `Right (set_dir pos dir acc))
      | '.' -> 
        let new_acc = (set_dir pos dir acc) in 
        step_beam (walk pos 1 dir) dir new_acc
      | _ -> failwith "wrong")
  in
  let r = step_beam (0, 0) `Right result in
  r

let char_grid_of_string s = 
  String.split_on_char '\n' s 
  |> Pvector.of_list 
  |> Pvector.map (fun s -> String.to_seq s |> List.of_seq |> Pvector.of_list) 
  
let count_excited = Pvector.fold_left (fun acc row -> Pvector.fold_left (fun acc state -> acc + excitement state) acc row) 0
let () = 
count_excited (dfs (char_grid_of_string {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|}))
|> Printf.printf "score: %d\n";