(* open Aoc.Public *)

(* let bin_search target vec = 
  let rec h lo hi = 
    if lo = hi then lo else
    (* ind skews low, i.e. when even is on the left *)
    let ind = (hi + lo) / 2 in 
    let value = vec --> ind in 
    if value = target then ind 
    else if value < target then h (ind + 1) hi
    else h lo ind
  in
  h 0 (Pvector.len vec) |> min (Pvector.len vec - 1) *)

let instructions str =
  String.split_on_char '\n' str 
  |> List.map (fun s -> 
    match String.split_on_char ' ' s with 
    | [dir; amt; _] -> (String.get dir 0, int_of_string amt)
    | _ -> failwith "bad instruction")

let apply_inst (x, y) (dir, amt) = 
  match dir with 
  | 'L' -> (x - amt, y)
  | 'R' -> (x + amt, y)
  | 'U' -> (x, y + amt)
  | 'D' -> (x, y - amt)
  | _ -> failwith "bad instruction"


(* let ortho d1 d2 = (d1 <> d2) && ((d1 = 'R' || d1 = 'L') <> (d2 = 'L' || d2 = 'R')) *)
let is_horiz d = d = 'R' || d = 'L'

(* direction, stack *)
type region = { start : int; count : int; perp_pos : int; }
type stack = { direction : char; stack : region list }
type stacks = { horizontal : stack; vertical : stack }

let add_to_stack stack start count perp_pos = { start = start; count = count; perp_pos = perp_pos }::stack

(* maybe sum up candidate areas and  *)
let rec erase_from_stack stack start count perp_pos sign = 
  match stack with 
  | region::rest -> 
    (* Printf.printf "erasing (%d, %d) from (%d, %d), sign %d\n" start count region.start region.count sign; *)
    if region.start + sign * region.count <> start then failwith "erro!" else 
    let remaining = region.count - count in 
    let removed_ct = min region.count count in 
    let perp_diff = abs (perp_pos - region.perp_pos) in 
    let acc = removed_ct * perp_diff in
    Printf.printf "removed_ct=%d, perp_diff=%d, acc %d\n" removed_ct perp_diff acc;
    if remaining > 0 then 
      { region with count = remaining }::rest
    else if remaining = 0 then rest
    else erase_from_stack rest region.start (count - region.count) perp_pos sign
  | _ -> failwith "empty!"

(* let print_stack stk = 
  let stack_str = List.fold_left (fun s nxt -> 
    s ^ "(start=" ^ string_of_int nxt.start ^ ", count=" ^ string_of_int nxt.count ^ ", perp_pos=" ^ string_of_int nxt.perp_pos ^ ")")
    "" stk.stack in 

  Printf.printf "DIR: %c, values: %s\n" stk.direction stack_str *)

let apply_inst (stacks, (x,y)) (dir, amt) = 
  let stack = if is_horiz dir then stacks.horizontal else stacks.vertical in 
  let stack = if stack.direction = ' ' then { stack with direction = dir } else stack in
  let start, perp_pos = if is_horiz dir then x, y else y, x in 
  let sign = if stack.direction = 'L' || stack.direction = 'D' then -1 else 1 in 
  (* Printf.printf "inst: (%c, %d)\n" dir amt; *)
  let new_stack = 
    { stack with stack = 
      if dir = stack.direction then 
        add_to_stack stack.stack start amt perp_pos
      else
        (* think we need to cap the perp_pos *)
        erase_from_stack stack.stack start amt perp_pos sign
    }
  in
  (* Printf.printf "after %s:\n" (if dir = stack.direction then "adding" else "erasing" ); *)
  (* print_stack new_stack; *)
  let result = if is_horiz dir then { stacks with horizontal = new_stack } else { stacks with vertical = new_stack } in 
  result, apply_inst (x,y) (dir,amt)

(* even simpler, maybe just a [or two] stack[s] of regions [for the orthogonal basis of 2d plane] *)
(* basically, direction we're painting [angular direction] *)
(* whenever we go against that we peek/maybe pop off the stack and see what we're canceling out / trimming *)
(* if it's trimmed to zero, pop again *)

(* !non-overlapping! "painted" lines and their perpendicular place *)
(* since disjoint, we can sort by either start or end [or maintain sorted list containing both, using indices to discriminate?] *)
(* i.e. [0,3] at row 3 *)

(* width, height *)
let process_stack insts = 
  List.fold_left apply_inst ({ 
    horizontal = { direction = ' '; stack = [] }; 
    vertical   = { direction = ' '; stack = [] } 
  }, (0, 0)) insts

let () =
ignore @@ process_stack (instructions "R 6 (#70c710)
D 5 (#0dc571)
L 2 (#5713f0)
D 2 (#d2c081)
R 2 (#59c680)
D 2 (#411b91)
L 5 (#8ceee2)
U 2 (#caa173)
L 1 (#1b58a2)
U 2 (#caa171)
R 2 (#7807d2)
U 3 (#a77fa3)
L 2 (#015232)
U 2 (#7a21e3)");