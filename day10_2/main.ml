module CharSet = Set.Make(Char)
module IntSet = Set.Make(Int)
module IntMap = Map.Make(Int)
module CharMap = Map.Make(Char)

let row_len input = (String.index_opt input '\n' |> Option.value ~default:(String.length input)) + 1

let walk_pos i row_len size (x,y) = 
  let pos = i + x + row_len * -y in 
  if pos > size || pos < 1 then
    None
  else
    Some pos

let connections = 
  [
    ('|', [(0, 1); (0, -1)]);
    ('-', [(1, 0); (-1, 0)]);
    ('F', [(0, -1); (1, 0)]);
    ('7', [(-1, 0); (0, -1)]);
    ('J', [(-1, 0); (0, 1)]);
    ('L', [(1, 0); (0, 1)]);
    ('S', [(1,0);(-1,0);(0,1);(0,-1)])
  ] |> List.fold_left (fun map (c, steps) -> CharMap.add c steps map) CharMap.empty

module TwoListQueue = struct
  (* AF: (f, b) represents the queue f @ (List.rev b).
     RI: given (f, b), if f is empty then b is empty. *)
  type 'a t = 'a list * 'a list

  let empty : 'a t = [], []

  let is_empty ((f, _) : 'a t) = 
    f = []

  let enq x (f, b) =
    if f = [] then [x], []
    else f, x :: b

  let front (f, _) = 
    List.hd f 

  let deq (f, b) =
    match List.tl f with
    | [] -> List.rev b, []
    | t -> t, b
end

let second (_, b) = b

let get_connections pos input_len input =
  let rl = row_len input in 
  let c = input.[pos] in
  let deltas = CharMap.find c connections in 
  List.combine deltas @@ List.map (walk_pos pos rl input_len) deltas 
  |> List.filter_map (fun (d, p) -> Option.map (fun pp -> (d, pp)) p)
  |> List.find (
    fun ((dx, dy), p) -> 
      CharMap.find_opt input.[p] connections 
      |> Option.map (List.mem (-dx, -dy)) 
      |> Option.value ~default:false)
  |> List.map (fun ((_, dy), p) -> (dy, p))

let rec dfs input input_len 

let rec bfs input input_len (queue, visited) = 
  if TwoListQueue.is_empty queue then 
    visited
  else
    (* add 'opposite' : bool to queue tuple. indicating loop direction *)
    let (pos, depth, y_dir) = TwoListQueue.front queue in
    let neighbors = get_connections pos input_len input in
    let new_queue = 
      neighbors
      |> List.filter (fun (_, p) -> IntMap.mem p visited |> not)
      |> List.fold_left (
        fun q (dy, n) -> 
          TwoListQueue.enq (n, depth + 1, 
          if input.[n] = '-' then 
            0 else 
          if dy != 0 then 
            dy
          else
            y_dir) q) (TwoListQueue.deq queue) in
    bfs input input_len (new_queue, IntMap.add pos y_dir visited)

let count visited input = 
  let ln = String.length input in
  let rec h p (sum, mult) = 
    if p = ln then 
      sum
    else 
      (* if input.[p] = '\n' then h (p + 1) (sum, 0) else *)
      if IntMap.mem p visited then h (p + 1) (sum, mult + IntMap.find p visited |> Int.min 1 |> Int.max 0) else h (p + 1) (sum + (1 * mult), mult) in 
  h 0 (0, 0)

let input = "FF7FSF7F7F7F7F7F---7
L|LJ||||||||||||F--J
FL-7LJLJ||||||LJL-77
F--JF--7||LJLJIF7FJ-
L---JF-JLJIIIIFJLJJ7
|F|F-JF---7IIIL7L|7|
|FFJF7L7F-JF7IIL---7
7-L-JL7||F7|L7F-7F7|
L.L7LFJ|||||FJL7||LJ
L7JLJL-JLJLJL--JLJ.L"

let () = 
  let set = bfs input (String.length input) (TwoListQueue.empty |> TwoListQueue.enq (String.index input 'S', 0, 0), IntSet.empty) in 
  count (fun p -> IntSet.mem p set) input |> Printf.printf "%d\n"