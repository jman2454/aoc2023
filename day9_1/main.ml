open Aoc.Util

let step ints = 
  let (_, diffs) = List.fold_left (fun (prev, diffs) next -> (next, diffs@[(next - prev)])) (0, []) ints in 
  List.tl diffs

let deconstruct series =
  let rec h ints history = 
    let (_, stop) = List.fold_left (fun (prev, mtch) next -> (next, mtch && prev = next)) (List.hd ints, true) ints in 
    if stop then 
      history
    else
      let l = step ints in
      h l (l::history) in
  h series [series]

let get_next series = 

let () = parse_int_list "10 13 16 21 30 45" |> deconstruct |> List.hd |> print_int_list