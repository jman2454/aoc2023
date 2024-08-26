(* for now, since we don't have (efficient) pvector removals/subvectors *)
(* we'll just keep track of truncation and append vs. set appropriately *)

(* `array`, `count` under precondition that `count < Pvector.len array` *)
type ('a, 'b) arr = (('a * 'b) option Pvector.t * int)

let (-->) = Pvector.(-->)
let (<--) = Pvector.(<--)

let empty : ('a, 'b) arr = Pvector.make_vec 0 None, 0

let append value (tree, count) = 
  if count < Pvector.len tree then 
    Pvector.set count (Some(value)) tree, count + 1
  else
    Pvector.append (Some(value)) tree, count + 1

let pop_end (tree, count) = (tree, count - 1)

let last_node (tree, count) = tree --> (count - 1)
let last_pos (_, count) = count - 1
let parent_pos pos = (pos - 1) / 2
let left_child_pos pos = pos * 2 + 1
let right_child_pos pos = pos * 2 + 2
let get_node pos (tree, count) = if pos < count && pos >= 0 then tree --> pos else failwith "out of vounds"
let set_node pos value (tree, count) = if pos < count && pos >= 0 then ((tree, pos)) <-- Some(value), count else failwith "out of vounsd"

let rec bubble_up pos tree = 
  let node = get_node pos tree in 
  if node = None then failwith "bubbling none??" else
  let (n_prio, n_val) = Option.get node in 
  let p_pos = parent_pos pos in 
  if p_pos = pos then tree else
  match get_node p_pos tree with 
  | None -> tree
  | Some (p_prio, _) when p_prio > n_prio -> tree
  | Some (p_prio, p_val) -> 
    set_node p_pos (n_prio, n_val) tree
    |> set_node pos (p_prio, p_val)
    |> bubble_up p_pos

let len (_, count) = count

let max_node_pos pos1 pos2 tree = 
  if pos1 >= len tree then if pos2 >= len tree then None else Some(pos2) else 
  if pos2 >= len tree then Some(pos1) else
  Option.map (fun (prio1, _) -> 
    Option.map (fun (prio2, _) -> if prio1 > prio2 then pos1 else pos2) (get_node pos2 tree) 
  ) (get_node pos1 tree) |> Option.join

let rec bubble_down pos tree = 
  let node = get_node pos tree in 
  if node = None then failwith "bubbling none??" else
  let (n_prio, n_val) = Option.get node in 
  let l_pos = left_child_pos pos in 
  let r_pos = right_child_pos pos in 
  let max_pos_opt = max_node_pos l_pos r_pos tree in 
  if max_pos_opt = None then tree else 
  let max_pos = Option.get max_pos_opt in 
  if max_pos >= len tree then tree else
  match get_node max_pos tree with 
  | None -> tree
  | Some(c_prio, _) when n_prio > c_prio -> tree
  | Some(c_prio, c_val) -> 
    set_node max_pos (n_prio, n_val) tree
    |> set_node pos (c_prio, c_val)
    |> bubble_down max_pos

let push value (tree : ('a, 'b) arr) = 
  let new_tree = append value tree in 
  bubble_up (last_pos new_tree) new_tree

let peek tree = 
  match get_node 0 tree with 
  | None -> failwith "empty!"
  | Some (_, value) -> value

let pop tree = 
  match get_node (last_pos tree) tree with 
  | None -> failwith "empty!"
  | Some (prio, value) ->
    bubble_down 0 (set_node 0 (prio, value) tree)