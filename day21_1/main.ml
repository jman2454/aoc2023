(* reachable by BFS, but also if positive & even remaning count then include self *)

let dimensions grid = 
  String.length (String.split_on_char '\n' grid |> List.hd), String.index grid '\n' + 1

let pos_to_index (row, col) width = row * width + col

module IntMap = Map.Make(Int)

let neighbors (row, col) grid width = 
  [ (row + 1, col); (row - 1, col); (row, col + 1); (row, col - 1) ]
  |> List.filter (fun pos -> 
    let ind = pos_to_index pos width in 
    ind >= 0 && ind < String.length grid && match String.get grid ind with 
    | '\n' | '#' -> false
    | _ -> true
  )

let rec bfs q grid width (seen, acc) = 
  if Aoc.Queue.is_empty q then (Printf.printf "empty"; seen, acc) else 
  let (row, col, remaining) = Aoc.Queue.front q in 
  let index = pos_to_index (row, col) width in 
  if IntMap.mem index seen then bfs (Aoc.Queue.deq q) grid width (seen, acc) else 
  if remaining = 0 then bfs (Aoc.Queue.deq q) grid width (IntMap.add index () seen, acc + 1) else
  let seen, acc = if remaining mod 2 = 0 then IntMap.add index () seen, acc + 1 else seen, acc in 
  let q = List.fold_left (fun q pos -> Aoc.Queue.enq (fst pos, snd pos, remaining - 1) q) (Aoc.Queue.deq q) (neighbors (row, col) grid width) in 
  bfs q grid width (seen, acc)

let () = 
let grid = "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........." in
let _, width = dimensions grid in 

let s_pos = String.index grid 'S' in 
bfs (Aoc.Queue.empty |> Aoc.Queue.enq (s_pos / width, s_pos mod width, 64)) grid width (IntMap.empty, 0)
|> snd
|> Printf.printf "count: %d\n";