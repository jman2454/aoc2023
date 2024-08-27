open Aoc.Public

(* let get_opt (row, col) v = 
  if row < 0 || col < 0 then None else
  if row < Pvector.len v && col < (Pvector.len (v --> row)) then 
    Some((v --> row) --> col)
  else
    None *)

(* let set (row, col) value grid = 
  (grid, row) <-- ((grid --> row, col) <-- value) *)

(* coords are double-inverted; i.e. row index corresponds to y, and low row indices are high y values *)
(* let walk (row, col) amt = function 
| `Left -> (row, col - amt)
| `Right -> (row, col + amt)
| `Up -> (row - amt, col)
| `Down -> (row + amt, col) *)

let char_grid_of_string s = 
  String.split_on_char '\n' s 
  |> Pvector.of_list 
  |> Pvector.map (fun s -> String.to_seq s |> List.of_seq |> Pvector.of_list)

(* let dijkstra_modified grid = 
   *)
  (* let q = Pvector.fold_lefti (
    fun row row_i (pq, id) -> 
      Pvector.fold_lefti (fun _ col_i (pq, id) -> 
        Aoc.Pqueue.push (Int.min_int, (row_i, col_i))) (pq, id) row
  ) *)
  
  
(* let count_excited = Pvector.fold_left (fun acc row -> Pvector.fold_left (fun acc state -> acc + excitement state) acc row) 0 *)
let () = 
(* count_excited  *)
Pvector.string_of_grid ((char_grid_of_string {|.|...\....
|.-.\.....
.....|-...
........|.
..........
.........\
..../.\\..
.-.-/..|..
.|....-|.\
..//.|....|})) (String.make 1)
|> Printf.printf "score: %s\n"