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

let get_neighbor pos input_len input pred =
  let rl = row_len input in 
  let c = input.[pos] in
  let deltas = CharMap.find c connections in 
  List.combine deltas @@ List.map (walk_pos pos rl input_len) deltas 
  |> List.filter_map (fun (d, p) -> Option.map (fun pp -> (d, pp)) p)
  |> List.find_opt (
    fun ((dx, dy), p) -> 
      CharMap.find_opt input.[p] connections 
      |> Option.map (List.mem (-dx, -dy)) 
      |> Option.value ~default:false
      |> (&&) (pred p))
  |> Option.map (fun ((_, dy), p) -> (dy, p))

let rec dfs input input_len (in_dy, pos) visited = 
  if IntMap.mem pos visited then 
    visited
  else
    let opt = 
      get_neighbor pos input_len input (
        fun p -> 
          (IntMap.mem p visited |> not) || input.[p] = 'S' && IntMap.exists (fun k _ -> input.[k] != 'S') visited
      ) 
    in
    match opt with 
    | None -> visited
    | Some (out_dy, n_pos) ->
      let mark = (if input.[pos] <> 'S' && out_dy = 0 && in_dy <> 0 then in_dy else out_dy) in 
      let new_visited = IntMap.add pos mark visited in
      if input.[n_pos] = 'S' then 
        if out_dy <> 0 then IntMap.add n_pos out_dy new_visited else new_visited
      else
        dfs input input_len (out_dy, n_pos) new_visited

let count visited input = 
  let ln = String.length input in
  let rec h p (sum, mult, p_d) = 
    if p = ln then 
      sum
    else 
      if IntMap.mem p visited then 
        let dir = IntMap.find p visited in 
        let new_mult = if p_d <> dir then (mult + dir) |> Int.min 1 |> Int.max (-1) else mult in
        h (p + 1) (sum, new_mult, if dir <> 0 then dir else p_d)
      else 
        h (p + 1) (sum + (if mult <> 0 then 1 else 0), mult, 0) in 
  h 0 (0, 0, 0)

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
  let set = dfs input (String.length input) (0, String.index input 'S') IntMap.empty in 
  count set input |> Printf.printf "%d\n"