include Aoc.Pvector
module Pvector = Aoc.Pvector

let rotate s = 
  let row_len = String.index s '\n' + 1 in
  let col_count = row_len - 1 in
  let row_count = String.fold_left (fun sum nxt -> sum + (if nxt = '\n' then 1 else 0)) 1 s in
  let rec extract_col rowi coli acc = 
    if rowi = row_count then 
      acc
    else
      extract_col (rowi + 1) coli (acc ^ String.make 1 @@ String.get s (rowi * row_len + coli))
  in
  let rec h i acc = 
    if i = col_count then 
      acc
    else
      h (i + 1) (extract_col 0 (col_count - 1 - i) ""::acc)
  in
  h 0 []

let one_diff s1 s2 = 
  let rec h i1 i2 found = 
    if i1 >= String.length s1 then 
      found
    else if String.get s1 i1 = String.get s2 i2 then 
      h (i1 + 1) (i2 + 1) found
    else if not found then 
      h (i1 + 1) (i2 + 1) true
    else
      false
  in
  h 0 0 false

(* modify to find the first reflection that requires at least one change *)

let find_reflections lines = 
  let rec h lines stk consume line_i changed = 
    match lines with 
    | line::l_rest -> 
      (match stk with 
      | nxt::s_rest -> 
          (
          let match_here = 
            if line = nxt then
              h l_rest s_rest (if consume = 0 then line_i else consume) (line_i + 1) changed
            else if not changed && one_diff line nxt then
              h l_rest s_rest (if consume = 0 then line_i else consume) (line_i + 1) true
            else 0
          in 
          match_here + if consume <> 0 then 0 else h l_rest (line::stk) 0 (line_i + 1) changed)
      | [] -> 
        (if consume <> 0 then (if changed then consume else 0)
        else h l_rest (line::stk) 0 (line_i + 1) changed))
    | [] -> 
      if changed then consume else 0
  in h lines [] 0 0 false

let do_horiz inp =
  (inp 
|> String.split_on_char '\n'
|> find_reflections) * 100

let do_virt inp =
inp 
|> rotate
|> find_reflections

let make_chunks input = 
  let all = String.split_on_char '\n' input in 
  let rec h remaining curr acc = 
    match remaining with 
    | ""::rest -> h rest [] @@ curr::acc
    | s::rest -> h rest (s::curr) acc
    | [] -> if curr <> [] then curr::acc else acc
  in
  h all [] []

let input = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#.

#...##..#
#....#..#
..##..###
#####.##.
#####.##.
..##..###
#....#..#"

let () = 
make_chunks input
|> List.map (List.fold_left (fun acc nxt_line -> nxt_line ^ (if acc <> "" then "\n" ^ acc else "")) "")
|> List.map (fun block -> do_horiz block + (1 * do_virt block))
|> List.fold_left (+) 0
|> Printf.printf "Result: %d\n"