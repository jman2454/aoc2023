(* for now, since we don't have (efficient) pvector removals/subvectors *)
(* we'll just keep track of truncation and append vs. set appropriately *)

(* `array`, `count` under precondition that `count < Pvector.len array` *)
module IntMap = Map.Make(Int)

type ('a, 'b) t = { 
  heap : (('a * 'b) * int) option Pvector.t;
  positions : int IntMap.t;
  count : int;
  next_id : int;
}

let (-->) = Pvector.(-->)
let (<--) = Pvector.(<--)

let empty : ('a, 'b) t = { 
  heap = Pvector.make_vec 0 None; 
  count = 0; 
  next_id = 1; 
  positions = IntMap.empty; 
}

(* 
  would sadly be good to store the position in the heap as well, allows for backreferences
  which is useful for popping/removal 
*)
let append value pq = 
  let new_tree = 
    if pq.count < Pvector.len pq.heap then 
      Pvector.set pq.count (Some(value, pq.next_id)) pq.heap
    else
      Pvector.append (Some(value, pq.next_id)) pq.heap
  in {
    heap = new_tree;
    count = pq.count + 1; 
    next_id = pq.next_id + 1; 
    positions = IntMap.add pq.next_id pq.count pq.positions 
  }, pq.next_id

let last_pos pq = pq.count - 1
let parent_pos pos = (pos - 1) / 2
let left_child_pos pos = pos * 2 + 1
let right_child_pos pos = pos * 2 + 2
let get_node pos pq = 
  if pos < pq.count && pos >= 0 then 
    pq.heap --> pos
  else failwith ("out of vounds: " ^ (string_of_int pos))

(* we don't change any ID mappings in this, simply an array set *)
let set_node pos value pq = 
  if pos < pq.count && pos >= 0 then 
    { pq with heap = (pq.heap, pos) <-- Some(value) }
  else 
    failwith "out of vounsd"

(* not done, need to remove position mapping *)
let pop_end pq = 
  match get_node (last_pos pq) pq with 
  | None -> pq
  | Some((_, id)) -> 
    { pq with count = pq.count - 1; positions = IntMap.remove id pq.positions }

let rec bubble_up pos pq = 
  let node = get_node pos pq in 
  if node = None then failwith "bubbling none??" else
  let ((n_prio, n_val), n_id) = Option.get node in 
  let p_pos = parent_pos pos in 
  if p_pos = pos then pq else
  match get_node p_pos pq with 
  | None -> pq
  | Some ((p_prio, _), _) when p_prio >= n_prio -> pq
  | Some ((p_prio, p_val), p_id) -> 
    let next_q =  
      set_node p_pos ((n_prio, n_val), n_id) pq
      |> set_node pos ((p_prio, p_val), p_id)
      |> fun q -> { q with positions = IntMap.add n_id p_pos q.positions |> IntMap.add p_id pos }
    in
    bubble_up p_pos next_q

let len pq = pq.count

let max_node_pos pos1 pos2 pq = 
  if pos1 >= len pq then if pos2 >= len pq then None else Some(pos2) else 
  if pos2 >= len pq then Some(pos1) else
  Option.map (fun ((prio1, _), _) -> 
    Option.map (fun ((prio2, _), _) -> if prio1 > prio2 then pos1 else pos2) (get_node pos2 pq) 
  ) (get_node pos1 pq) |> Option.join

let rec bubble_down pos tree = 
  let node = get_node pos tree in 
  if node = None then failwith "bubbling none??" else
  let ((n_prio, n_val), n_id) = Option.get node in 
  let l_pos = left_child_pos pos in 
  let r_pos = right_child_pos pos in 
  let max_pos_opt = max_node_pos l_pos r_pos tree in 
  if max_pos_opt = None then tree else 
  let max_pos = Option.get max_pos_opt in 
  if max_pos >= len tree then tree else
  match get_node max_pos tree with 
  | None -> tree
  | Some((c_prio, _), _) when n_prio >= c_prio -> tree
  | Some((c_prio, c_val), c_id) -> 
    let next_q = 
      set_node max_pos ((n_prio, n_val), n_id) tree
      |> set_node pos ((c_prio, c_val), c_id)
      |> fun q -> { q with positions = q.positions |> IntMap.add n_id max_pos |> IntMap.add c_id pos }
    in
    bubble_down max_pos next_q

(* also returns element's id, which can be used to fast-find the element later on *)
let push value (pq : ('a, 'b) t) = 
  let pq, id = append value pq in 
  bubble_up (last_pos pq) pq, id

let peek pq = 
  match get_node 0 pq with 
  | None -> failwith "empty!"
  | Some ((_, value), _) -> value

let pop pq = 
  match get_node (last_pos pq) pq with 
  | None -> failwith "empty!"
  | Some (tup, id) ->
    let popped_id = get_node 0 pq |> Option.get |> snd in 
    (* special case when popping empty *)
    if len pq = 1 then { pq with count = 0; positions = IntMap.empty } else
    bubble_down 0 (
      set_node 0 (tup, id) pq 
      |> fun q -> { 
        q with 
          positions = IntMap.add id 0 q.positions |> IntMap.remove popped_id; 
          count = q.count - 1 
      }
    )

let increase_key id tree new_priority = 
  let pos = IntMap.find_opt id tree.positions in 
  if Option.is_none pos then failwith "not found!" else 
  let pos = Option.get pos in 
  match get_node pos tree with 
  | None -> failwith "invalid pos"
  | Some ((_, value), id) -> 
    set_node pos ((new_priority, value), id) tree
    |> bubble_up pos