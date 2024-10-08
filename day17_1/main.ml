open Aoc.Public

let get_opt (row, col) v = 
  if row < 0 || col < 0 then None else
  if row < Pvector.len v && col < (Pvector.len (v --> row)) then 
    Some((v --> row) --> col)
  else
    None

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

let perpendicular = function | Left | Right -> [Up; Down] | Up | Down -> [Left; Right]
let max_consecutive = 3
let valid_headings last_dir last_dir_count = perpendicular last_dir @ (if last_dir_count < max_consecutive then [last_dir] else [])

let get_neighbors pos headings grid = 
  List.map (fun dir -> dir, get_opt (walk pos 1 dir) grid) headings 
  |> List.filter_map (fun (dir, x) -> Option.map (fun x -> (dir, x)) x)

let pick_dir (l,r,u,d) = function Left -> l | Right -> r | Up -> u | Down -> d

let add_dir pos dir pq = 
  let pq, id1 = Aoc.Pqueue.push (Int.min_int, (pos, dir, 1)) pq in
  let pq, id2 = Aoc.Pqueue.push (Int.min_int, (pos, dir, 2)) pq in 
  let pq, id3 = Aoc.Pqueue.push (Int.min_int, (pos, dir, 3)) pq in 
  pq, Pvector.of_list [id1;id2;id3]

let update_pos_priorities pq ids_costs (r,c) prio = 
  let (lids, rids, uids, dids), _ = ids_costs --> r --> c in 
  let pq = Pvector.fold_left (fun pq id -> Aoc.Pqueue.update_priority id pq prio) pq lids in 
  let pq = Pvector.fold_left (fun pq id -> Aoc.Pqueue.update_priority id pq prio) pq rids in 
  let pq = Pvector.fold_left (fun pq id -> Aoc.Pqueue.update_priority id pq prio) pq uids in 
  Pvector.fold_left (fun pq id -> Aoc.Pqueue.update_priority id pq prio) pq dids

(* we should store indices too, so we want fold_left_mapi *)
let dijkstra_modified grid = 
  let target = (Pvector.len grid - 1, Pvector.len (grid --> 0) - 1) in 
  let ids_costs, pq = Pvector.fold_left_mapi (fun row_i pq row -> 
    Pvector.fold_left_mapi 
      (fun col_i pq el -> 
        let pq, l_ids = add_dir (row_i, col_i) Left pq in 
        let pq, r_ids = add_dir (row_i, col_i) Right pq in 
        let pq, u_ids = add_dir (row_i, col_i) Up pq in 
        let pq, d_ids = add_dir (row_i, col_i) Down pq in 
        (((l_ids, r_ids, u_ids, d_ids), el), pq))
      pq row
    ) Aoc.Pqueue.empty grid
  in 
  let pq = update_pos_priorities pq ids_costs (0, 0) 0 in
  let rec process pq = 
    if Aoc.Pqueue.len pq = 0 then 
      failwith "did not find!"
    else
      let cost = Aoc.Pqueue.peek_prio pq in 
      let (pos, dir, dir_ct) = Aoc.Pqueue.peek pq in 
      if pos = target then
        -cost
      else
        let headings = valid_headings dir dir_ct in 
        let neighbors = 
          get_neighbors pos headings ids_costs
          |> List.filter_map (fun (n_dir, (ids, el)) -> 
            let new_dir_ct = if dir = n_dir then dir_ct + 1 else 1 in 
            let id = pick_dir ids n_dir --> (new_dir_ct - 1) in 
            if Aoc.Pqueue.contains id pq then Some(id, el, new_dir_ct) else None) 
        in
        List.fold_left (fun q (id, el, new_dir_ct) -> 
          Aoc.Pqueue.map_id id q (
            fun neg_curr_cost (n_pos, n_dir, n_dir_ct) ->               
              let neg_walk_cost = cost - el in 
              let cost, ct = if neg_walk_cost > neg_curr_cost then neg_walk_cost, new_dir_ct else neg_curr_cost, n_dir_ct in 
              cost, (n_pos, n_dir, ct)
          )
        ) (Aoc.Pqueue.pop pq) neighbors
        |> process
  in
  process pq
  
let () = 
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