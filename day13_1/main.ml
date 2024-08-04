(* type trie = 
| Node of trie * trie * bool (* . and #, then optional match *)
| Null

let is_match = function | Node (_, _, b) -> b |  _ -> false

let rec add s i t = 
  if i >= String.length s then 
    (* we're done, and no more characters to care about *)
    Node(Null, Null, true)
  else
    let c = String.get s i in 
    match t with 
    | Node (t1, t2, b) -> 
      if c = '.' then 
        Node(add s (i + 1) t1, t2, b)
      else 
        Node(t1, add s (i + 1) t2, b)
    | Null -> 
      (* make a new node *)
      if c = '.' then 
        Node(add s (i + 1) Null, Null, false)
      else
        Node(Null, add s (i + 1) Null, false)

let rec match_s s i t = 
  if i >= String.length s then 
    is_match t
  else
    let c = String.get s i in 
    match t with 
    | Node (t1, t2, _) -> 
      if c = '.' then 
        match_s s (i + 1) t1
      else 
        match_s s (i + 1) t2
    | _ -> false

let add s t = add s 0 t
let match_s s t = match_s s 0 t

let rec to_s t = 
  match t with 
  | Null -> "Null"
  | Node(l, r, b) -> "Node(" ^ to_s l ^ ", " ^ to_s r ^ ", " ^ string_of_bool b ^ ")"

let test s t = 
  let t = add s t in 
  if not @@ match_s s t then 
    failwith "wow"
  else
    t *)
include Aoc.Pvector
module Pvector = Aoc.Pvector

let rotate s = 
  let row_len = String.index s '\n' + 1 in
  let col_len = String.fold_left (fun sum nxt -> sum + (if nxt = '\n' then 1 else 0)) 1 s in
  let rec extract_col rowi coli acc = 
    if rowi = col_len then 
      acc
    else
      extract_col (rowi + 1) coli (acc ^ String.make 1 @@ String.get s (rowi * row_len + coli))
  in
  let rec h i acc = 
    if i = col_len then 
      acc
    else 
      h (i + 1) (extract_col 0 (col_len - 1 - i) ""::acc)
  in
  h 0 []

(* let print_list l = l |> Pvector.of_list |> Pvector.to_str (fun s -> s) |> Printf.printf "%s\n" *)

let find_reflections lines = 
  let rec h lines stk consume = 
    match lines with 
    | line::l_rest -> 
      (match stk with 
      | nxt::s_rest -> 
          let sub = if line = nxt then h l_rest s_rest true else 0 in 
          sub + h l_rest (line::stk) false
      | [] -> 
        (if consume && l_rest = [] then 1 else  0) + h l_rest (line::stk) false)
    | [] -> if consume then 1 else 0
  in h lines [] false

let input = "#.##..##.
..#.##.#.
##......#
##......#
..#.##.#.
..##..##.
#.#.##.#."

let () = 
input 
(* |> String.split_on_char '\n' *)
|> rotate
|> find_reflections 
|> Printf.printf "%d\n";
