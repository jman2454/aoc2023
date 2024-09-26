module IntMap = Map.Make(Int)
let dimensions grid = 
  String.length (String.split_on_char '\n' grid |> List.hd), String.index grid '\n' + 1

let neighbors index grid width = 
  [ index + width; index - width; index + 1; index - 1 ]
  |> List.filter (fun pos -> 
    if pos < 0 || pos >= String.length grid then false else
    match String.get grid pos with 
    | '\n' | '#' -> false
    | _ -> true
  )

let rec bfs q grid width (even, odd) = 
  if Aoc.Queue.is_empty q then even, odd else 
  let (index, remaining) = Aoc.Queue.front q in 
  let even_remaining = remaining mod 2 = 0 in 
  if IntMap.mem index (if even_remaining then even else odd) then bfs (Aoc.Queue.deq q) grid width (even, odd) else 
  if remaining = 0 then bfs (Aoc.Queue.deq q) grid width (IntMap.add index () even, odd) else
  let even, odd = if even_remaining then IntMap.add index () even, odd else even, IntMap.add index () odd in 
  let q = List.fold_left (fun q pos -> Aoc.Queue.enq (pos, remaining - 1) q) (Aoc.Queue.deq q) (neighbors index grid width) in 
  bfs q grid width (even, odd)

let n = 26501365
let s = 131
let diamond_diameter = (n / s) * 2 + 1
let diamond_radius = (diamond_diameter / 2) + 1
let single_edge_internal_ct = diamond_radius - 2

let rec get_counts ring_index even odd = 
  if ring_index = 0 then even + 1, odd else
  if ring_index mod 2 = 0 then
    get_counts (ring_index - 1) (4 * ring_index + even) odd
  else
    get_counts (ring_index - 1) even (4 * ring_index + odd)

let interior_count = get_counts (diamond_radius - 2) 0 0 

let reachable_from_edge edge remaining grid = 
  let height, width = dimensions grid in 
  let start_index = match edge with 
  | `Left -> 0 + (height / 2) * width
  | `Right -> (width - 2) + (height / 2) * width
  | `Top -> (width - 2) / 2 + 0
  | `Bottom -> (width - 2) / 2 + (height - 1) * width
  in
  bfs (Aoc.Queue.empty |> Aoc.Queue.enq (start_index, remaining)) grid width (IntMap.empty, IntMap.empty)
  |> if remaining mod 2 = 0 then fst else snd

let reachable_from_corner corner remaining grid = 
  let height, width = dimensions grid in 
  let start_index = match corner with 
  | `Left, `Top -> 0 + 0
  | `Right, `Top -> (width - 2) + 0
  | `Left, `Bottom -> 0 + (height - 1) * width
  | `Right, `Bottom -> (width - 2) + (height - 1) * width
  in
  bfs (Aoc.Queue.empty |> Aoc.Queue.enq (start_index, remaining)) grid width (IntMap.empty, IntMap.empty)
  |> if remaining mod 2 = 0 then fst else snd

let key_ct map = IntMap.fold (fun _ _ acc -> acc + 1) map 0

let combos edge_remaining corner_remaining grid = 
[ 
  IntMap.union (fun _ _ _ -> Some()) (reachable_from_edge `Bottom edge_remaining grid) (reachable_from_edge `Left edge_remaining grid);
  IntMap.union (fun _ _ _ -> Some()) (reachable_from_edge `Bottom edge_remaining grid) (reachable_from_edge `Right edge_remaining grid);
  IntMap.union (fun _ _ _ -> Some()) (reachable_from_edge `Top edge_remaining grid) (reachable_from_edge `Left edge_remaining grid);
  IntMap.union (fun _ _ _ -> Some()) (reachable_from_edge `Top edge_remaining grid) (reachable_from_edge `Right edge_remaining grid);

  reachable_from_corner (`Left, `Top) corner_remaining grid;
  reachable_from_corner (`Left, `Bottom) corner_remaining grid;
  reachable_from_corner (`Right, `Top) corner_remaining grid;
  reachable_from_corner (`Right, `Bottom) corner_remaining grid
]
|> List.mapi (fun ind map -> ((if ind > 3 then 1 else 0) + single_edge_internal_ct) * key_ct map)
|> List.fold_left (+) 0
|> (+) (reachable_from_edge `Left edge_remaining grid |> key_ct)
|> (+) (reachable_from_edge `Right edge_remaining grid |> key_ct)
|> (+) (reachable_from_edge `Bottom edge_remaining grid |> key_ct)
|> (+) (reachable_from_edge `Top edge_remaining grid |> key_ct)

let from_center grid = 
  let s_pos = String.index grid 'S' in
  let _, width = dimensions grid in
  bfs (Aoc.Queue.empty |> Aoc.Queue.enq (s_pos, 10000001)) grid width (IntMap.empty, IntMap.empty)

let reachable_interior grid = (
  from_center grid 
  |> fun (e, o) -> fst interior_count * (key_ct e) + snd interior_count * (key_ct o))

let () = 
let grid = {|...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........|} in 
reachable_interior grid + combos 131 65 grid
|> Printf.printf "Part 2: %d\n";