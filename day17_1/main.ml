open Aoc.Public

let get_opt (row, col) v = 
  if row < 0 || col < 0 then None else
  if row < Pvector.len v && col < (Pvector.len (v --> row)) then 
    Some((v --> row) --> col)
  else
    None

(* let set (row, col) value grid = 
  (grid, row) <-- ((grid --> row, col) <-- value) *)

type direction = Left | Right | Up | Down

(* coords are double-inverted; i.e. row index corresponds to y, and low row indices are high y values *)
let walk (row, col) amt = function 
| Left -> (row, col - amt)
| Right -> (row, col + amt)
| Up -> (row - amt, col)
| Down -> (row + amt, col)

let char_grid_of_string s = 
  String.split_on_char '\n' s 
  |> Pvector.of_list 
  |> Pvector.map (fun s -> String.to_seq s |> List.of_seq |> Pvector.of_list)

(* let flip (a,b) = (b,a) *)

(* position, shortest path cost, last direction, last direction count *)
(* type state = (int * int) * int * direction * int *)

let perpendicular = function | Left | Right -> [Up; Down] | Up | Down -> [Left; Right]
let max_consecutive = 3
let valid_headings last_dir last_dir_count = perpendicular last_dir @ (if last_dir_count < max_consecutive then [last_dir] else [])

let get_neighbors pos headings grid = 
  List.map (fun dir -> dir, get_opt (walk pos 1 dir) grid) headings 
  |> List.filter_map (fun (dir, x) -> Option.map (fun x -> (dir, x)) x)

let str_of_dir = function 
| Left -> "Left"
| Right -> "Right"
| Up -> "Up"
| Down -> "Down"

let str_of_pos (x,y) = "(" ^ (string_of_int x) ^ ", " ^ (string_of_int y) ^ ")"
let pick_dir (l,r,u,d) = function Left -> l | Right -> r | Up -> u | Down -> d

(* we should store indices too, so we want fold_left_mapi *)
let dijkstra_modified grid = 
  let target = (Pvector.len grid - 1, Pvector.len (grid --> 0) - 1) in 
  let ids_costs, pq = Pvector.fold_left_mapi (fun row_i pq row -> 
    Pvector.fold_left_mapi 
      (fun col_i pq el -> 
        let q, r_id = Aoc.Pqueue.push (Int.min_int, ((row_i, col_i), Right, 0)) pq in 
        let q, l_id = Aoc.Pqueue.push (Int.min_int, ((row_i, col_i), Left, 0)) q in 
        let q, u_id = Aoc.Pqueue.push (Int.min_int, ((row_i, col_i), Up, 0)) q in 
        let q, d_id = Aoc.Pqueue.push (Int.min_int, ((row_i, col_i), Down, 0)) q in 
        (((l_id, r_id, u_id, d_id), el), q))
      pq row
    ) Aoc.Pqueue.empty grid
  in 
  let (id1, id2, id3, id4), _ = ids_costs --> 0 --> 0 in 
  let pq = Aoc.Pqueue.update_priority id1 pq 0 in 
  let pq = Aoc.Pqueue.update_priority id2 pq 0 in 
  let pq = Aoc.Pqueue.update_priority id3 pq 0 in 
  let pq = Aoc.Pqueue.update_priority id4 pq 0 in 
  let rec process pq = 
    if Aoc.Pqueue.len pq = 0 then 
      failwith "did not find!"
    else
      let cost = Aoc.Pqueue.peek_prio pq in 
      let (pos, dir, dir_ct) = Aoc.Pqueue.peek pq in 
      
      (* let check_pos = (0, 5) in  *)
      (* if pos = check_pos then  *)
        Printf.printf "pos: %s, cost: %d, dir: %s, dir_ct: %d\n" (str_of_pos pos) cost (str_of_dir dir) dir_ct;
      (* Printf.printf "pos: %s, cost: %d\n" (str_of_pos pos) cost; *)
      
      if pos = target then
        -cost
      else
        let headings = valid_headings dir dir_ct in 

        (* if pos = check_pos then  *)
          (* Printf.printf "POS: %s\n" (str_of_pos pos);  *)
          (* List.iter (fun dir -> Printf.printf "heading: %s\n" (str_of_dir dir)) headings; *)
        
        let neighbors = 
          get_neighbors pos headings ids_costs 
          |> List.filter_map (fun (dir, (ids, el)) -> 
            let id = pick_dir ids dir in 
            if Aoc.Pqueue.contains id pq then Some(id, el) else None) 
        in
        List.fold_left (fun q (id, el) -> 
          Aoc.Pqueue.map_id id q (
            fun neg_curr_cost (n_pos, n_dir, n_dir_ct) ->               
              let neg_walk_cost = cost - el in 
              let new_dir_ct = if n_dir = dir then dir_ct + 1 else 1 in 
              (* if n_pos = check_pos then  *)
                (* Printf.printf "el: %d, source_pos: %s, maybe_cost: %d, curr_cost: %d\n" el (str_of_pos pos) neg_walk_cost neg_curr_cost; *)
              if neg_walk_cost > neg_curr_cost 
                || (neg_walk_cost = neg_curr_cost && new_dir_ct < dir_ct) 
                then
                (neg_walk_cost, (n_pos, n_dir, new_dir_ct))
              else
                neg_curr_cost, (n_pos, n_dir, n_dir_ct)
          )
        ) (Aoc.Pqueue.pop pq) neighbors
        |> process
  in
  process pq
  
(* let count_excited = Pvector.fold_left (fun acc row -> Pvector.fold_left (fun acc state -> acc + excitement state) acc row) 0 *)
let () = 
(* count_excited  *)
(char_grid_of_string {|2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533|})
|> Pvector.map (Pvector.map (fun c -> String.make 1 c |> int_of_string))
|> dijkstra_modified
|> Printf.printf "score: %d\n"