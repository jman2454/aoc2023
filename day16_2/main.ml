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

module HashedInput = 
struct
  type t = (int * int) * [ `Down | `Left | `Right | `Up ]
  let hash ((i1, i2), d) = 
    i1 * 37 
    + i2 * 21
    + match d with | `Left -> 2 | `Right -> 3 | `Up -> 5 | `Down -> 7
  
  let equal ((i1, i2), d1) ((i3, i4), d2) = i1 = i3 && i2 = i4 && d1 = d2
end

module InputHashTable = Hashtbl.Make(HashedInput)


let dfs grid = 
  let memo = ref (InputHashTable.create 100) in
  let activations = Pvector.map (Pvector.map (fun _ -> (0,0,0,0))) grid in
  let rec step_beam pos dir acc = 
    match InputHashTable.find_opt !memo (pos, dir) with 
    | Some count -> let prev, activations = acc in (prev + count, activations)
    | None -> 
      let (row, col) = pos in 
      let (count, activations) = 
        match get_opt pos grid with 
        | None -> 
          InputHashTable.add !memo (pos, dir) (0); acc
        | Some c -> 
          let (count, activations) = acc in 
          let new_count = if excitement (activations --> row --> col) > 0 then count else count + 1 in
          if is_active (activations --> row --> col) dir then (new_count, activations) else 
          let step = walk pos 1 in 
          InputHashTable.add !memo (pos, dir) (count - fst acc);
        ( match c with
          | '|' -> 
            (match dir with
            | `Left | `Right -> step_beam (step `Up) `Up (new_count, set_dir pos `Up activations |> set_dir pos `Down) |> step_beam (walk pos 1 `Down) `Down
            | `Up   | `Down  -> step_beam (step dir) dir (new_count, set_dir pos dir activations))
          | '-' -> 
            (match dir with
            | `Left | `Right -> step_beam (step dir) dir (new_count, set_dir pos dir activations)
            | `Up   | `Down  -> step_beam (step `Left) `Left (new_count, set_dir pos `Left activations |> set_dir pos `Right) |> step_beam (walk pos 1 `Right) `Right)
          | '/' -> 
            (match dir with
            | `Left  -> step_beam (step `Down)  `Down  (new_count, set_dir pos dir activations)
            | `Right -> step_beam (step `Up)    `Up    (new_count, set_dir pos dir activations)
            | `Up    -> step_beam (step `Right) `Right (new_count, set_dir pos dir activations)
            | `Down  -> step_beam (step `Left)  `Left  (new_count, set_dir pos dir activations))
          | '\\' -> 
            (match dir with
            | `Left  -> step_beam (step `Up)    `Up    (new_count, set_dir pos dir activations)
            | `Right -> step_beam (step `Down)  `Down  (new_count, set_dir pos dir activations)
            | `Up    -> step_beam (step `Left)  `Left  (new_count, set_dir pos dir activations)
            | `Down  -> step_beam (step `Right) `Right (new_count, set_dir pos dir activations))
          | '.' -> 
            let new_acc = new_count, (set_dir pos dir activations) in 
            step_beam (walk pos 1 dir) dir new_acc
          | _ -> failwith "wrong")
      in
      count, activations
  in
  let perp = function `Left | `Right -> `Down | `Up | `Down -> `Right in
  let rec max_over_dir dir start acc =
    memo := InputHashTable.create 100;
    match get_opt start activations with 
    | None -> acc
    | Some _ -> 
      let res, _ = step_beam start dir (0, activations) in 
      max_over_dir dir (walk start 1 (perp dir)) (max res acc)
  in
  max_over_dir `Right (0, 0) 0
  |> max_over_dir `Left (0, Pvector.len (activations --> 0) - 1)
  |> max_over_dir `Up (Pvector.len activations - 1, 0)
  |> max_over_dir `Down (0, 0)
  
let char_grid_of_string s = 
  String.split_on_char '\n' s 
  |> Pvector.of_list 
  |> Pvector.map (fun s -> String.to_seq s |> List.of_seq |> Pvector.of_list) 
  
(* let count_excited = Pvector.fold_left (fun acc row -> Pvector.fold_left (fun acc state -> acc + excitement state) acc row) 0 *)
let () = 
(* count_excited  *)
(dfs (char_grid_of_string {|.|...\....
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