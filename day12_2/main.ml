include Aoc

module Pvector = Aoc.Pvector

let (-->) = Pvector.(-->)
let (<--) = Pvector.(<--)

let parse line = 
  String.split_on_char ' ' line
  |> function 
  | [a; b] -> Pvector.of_string a, String.split_on_char ',' b |> List.map int_of_string |> Pvector.of_list
  | _ -> failwith "invalid"

let all_in_window start count vec pred = 
  let l = Pvector.len vec in
  let rec h ct acc = 
    if not acc then 
      false 
    else if ct = count || count <= 0 then 
      true
    else if start + ct >= l then 
      false
    else 
      h (ct + 1) (acc && pred @@ vec --> (start + ct))
  in
  h 0 true

let rec arrangements2 slots slot_i groups group_i memo = 
  let l = Pvector.len slots in 
  let g_l = Pvector.len groups in 
  let cached = (memo --> slot_i) --> group_i in 
  if cached <> -1 then 
    cached, memo
  else if group_i >= g_l then 
    let res = if all_in_window slot_i (l - slot_i) slots (fun c -> c <> '#') then 1 else 0 in 
    res, (memo, slot_i) <-- ((memo --> slot_i, group_i) <-- res)
  else if slot_i >= l then 
    0, (memo, slot_i) <-- ((memo --> slot_i, group_i) <-- 0)
  else
    let c = slots --> slot_i in 
    let dot_result, memo = if c <> '#' then arrangements2 slots (slot_i + 1) groups group_i memo else 0, memo in
    let g_len = groups --> group_i in 
    let next_slot = slot_i + g_len in 
    if all_in_window slot_i g_len slots (fun c -> c <> '.')
      && (next_slot >= l || slots --> next_slot <> '#') then 
      let res, memo = arrangements2 slots (min (next_slot + 1) (l)) groups (group_i + 1) memo in 
      let memo = (memo, slot_i) <-- ((memo --> slot_i, group_i) <-- dot_result + res) in 
      dot_result + res, memo
    else
      dot_result, memo

let blowup line = 
  match String.split_on_char ' ' line with 
  | [a;b] -> a ^ "?" ^ a ^ "?" ^ a ^ "?" ^ a ^ "?" ^ a
  ^ " " ^ 
  b ^ "," ^ b ^ "," ^ b ^ "," ^ b ^ "," ^ b
  | _ -> failwith "invalid input"

let input = "??? 1"

let () = 
  let make_memo slots groups = (Pvector.make_vec (Pvector.len slots + 1) (Pvector.make_vec (Pvector.len groups + 1) (-1))) in 
  String.split_on_char '\n' input
  |> List.fold_left (fun acc line -> 
    let (slots, groups) = parse (blowup line) in 
    let r, _ = arrangements2 slots 0 groups 0 (make_memo slots groups)
       in r + acc
    ) 0
  |> Printf.printf "ANSWER: %d\n";