(* now need to track how many dists I can add to myself (in my column) *)
(* that number is equal to however many slots the guy above me will slide + 1 - (number of Os above me) *)

let ezpz s = 
  let slen = String.length s in 
  let nl_ind = String.index s '\n' in 
  let lines = (slen + 1) / (nl_ind + 1) in

  let row_len = nl_ind + 1 in 

  let rec h i dist acc = 
    if i >= slen then acc else
    let c = String.get s i in 
    if c = '\n' then h (i + 1) (dist - 1) acc else 
    if c = 'O' then h (i + 1) dist (acc + dist) else 
    h (i + 1) dist acc
  in
  h 0 lines 0

let () = ezpz "OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#...." |> Printf.printf "%d\n";