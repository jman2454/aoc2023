open Aoc.Public

(* conj. should be a (int, bool IntMap.t) *)

module StringMap = Map.Make(String)
module IntMap = Map.Make(Int)

type node_state = FlipFlop of bool | Conjunction of int * int * bool IntMap.t | Broadcaster | Stop
type node = { index : int; name : string; children : int Pvector.t; state : node_state }

let parse_node str nodes map = 
  let (target, children) = match String.split_on_char '>' str with 
  | [ target; children ] -> (
      String.sub target 0 (String.length target - 2), 
      String.sub children 1 (String.length children - 1)
      |> String.split_on_char ','
      |> List.map String.trim
      |> Pvector.of_list
    )
  | _ -> failwith "bad input"
  in

  let target, state = match String.get target 0 with 
  | '%' -> String.sub target 1 (String.length target - 1), FlipFlop(false)
  | '&' -> String.sub target 1 (String.length target - 1), Conjunction((0, 0, IntMap.empty))
  | _ -> if target = "broadcaster" then target, Broadcaster else failwith "bad node"
  in

  Pvector.append (target, state, children) nodes, StringMap.add target (Pvector.len nodes) map

let resolve_nodes nodes map = 
  (* add implicit nodes *)
  let (nodes, map) = Pvector.fold_left (fun (nodes, map) (_, _, children) -> 
      Pvector.fold_left (fun (nodes, map) child_name -> 
        if StringMap.mem child_name map then (nodes, map) else 
        (Pvector.append (child_name, Stop, Pvector.empty) nodes, StringMap.add child_name (Pvector.len nodes) map)
      ) (nodes, map) children
    )
    (nodes, map) nodes
  in

  let find = fun c -> StringMap.find c map in 
  let nodes = Pvector.mapi (fun index (node_name, node_state, child_names) -> { 
    index = index; children = Pvector.map find child_names;
    state = node_state; name = node_name
  }) nodes
  in
  Pvector.fold_lefti (fun nodes i node -> 
    Pvector.fold_left (fun nodes c_i -> 
      let child = nodes --> c_i in 
      (nodes, c_i) <-- ({ child with state = 
        match child.state with 
        | Conjunction (ct, total_ct, parent_map) -> Conjunction((ct, total_ct + 1, IntMap.add i false parent_map))
        | _ -> child.state
      }))
      nodes node.children
  ) nodes nodes 

let print_nodes nodes = 
  Pvector.fold_left (fun () node -> 
    let child_names = Pvector.fold_left (fun s c_i -> s ^ ", " ^ (nodes --> c_i).name) "" node.children in
    let state_str = match node.state with 
    | Conjunction (ct, total_ct, parents) -> 
      let parents_str = IntMap.fold (fun p_i _ s -> s ^ ", " ^ (nodes --> p_i).name) parents "" in 
      "Conjunction(ct=" ^ string_of_int ct ^ ", total_ct=" ^ string_of_int total_ct ^ ", parents=[" ^ parents_str ^ "])"
    | FlipFlop on -> "FlipFlop(on=" ^ string_of_bool on ^ ")"
    | Broadcaster -> "Broadcaster"
    | Stop -> "Stop"
    in
    Printf.printf "name=%s, index=%d, children=[%s], state=%s\n" node.name node.index child_names state_str
  ) () nodes

let parse_input s = 
  let (nodes, map) = 
    List.fold_left (fun (nodes, map) row -> parse_node row nodes map) (Pvector.empty, StringMap.empty) (String.split_on_char '\n' s)
  in
  resolve_nodes nodes map

type pulse = { high : bool; source : int }

let process_pulse node_i nodes pulse = 
  let node = nodes --> node_i in 
  match node.state with 
  | Broadcaster -> nodes, Some({ high = pulse.high; source = node_i })
  | Conjunction (ct, total_ct, parent_map) -> 
    if not (IntMap.mem pulse.source parent_map) then failwith "unexpected" else
    let new_ct = 
      let was_high = IntMap.find pulse.source parent_map in 
      if was_high && not pulse.high then ct - 1 else 
      if not was_high && pulse.high then ct + 1 else 
      ct
    in
    let nodes = (nodes, node_i) <-- { 
      node with state = Conjunction(new_ct, total_ct, IntMap.add pulse.source pulse.high parent_map) 
    } in
    nodes, Some({ high = not (new_ct = total_ct); source = node_i })
  | FlipFlop on -> 
    if pulse.high then nodes, None else 
    (nodes, node_i) <-- { node with state = FlipFlop(not on) }, Some({ high = not on; source = node_i })
  | Stop -> nodes, None

let rec loop_q q nodes res index = 
  if Aoc.Queue.is_empty q then 
    (q, nodes, res)
  else
    let p = Aoc.Queue.front q in 
    let affected = (nodes --> p.source).children in 
    let res = res || (p.source = index && p.high) in 
    let (q, nodes) = Pvector.fold_left (fun (q, nodes) c_i -> 
      let (nodes, pulse_opt) = process_pulse c_i nodes p in 
      match pulse_opt with
      | None -> (q, nodes)
      | Some(pulse) -> (Aoc.Queue.enq pulse q, nodes)
    ) (Aoc.Queue.deq q, nodes) affected
    in
    loop_q q nodes res index

let find_index name nodes = Pvector.fold_left (fun ind node -> 
  if node.name = name then node.index else ind) (-1) nodes

let run_nodes nodes interest_index = 
  let broadcaster_index = find_index "broadcaster" nodes in
  let (_, nodes, did) = 
    loop_q (Aoc.Queue.empty |> Aoc.Queue.enq { high = false; source = broadcaster_index }) nodes false interest_index
  in 
  nodes, did

let find_high nodes interest_index = 
  let rec h ct nodes = 
    let (nodes, did) = run_nodes nodes interest_index in 
    if did then (ct + 1) else 
    h (ct + 1) nodes
  in

  h 0 nodes

let () = 
let nodes = parse_input {|%vg -> lf, vd
%dr -> kg
%cn -> mv, pt
%rq -> bk, gr
%vp -> lp, bk
%kg -> lv
%lv -> jc, tp
%sj -> rm, vd
%jc -> tp, qr
%km -> tp, dr
%jx -> cn
&vd -> tf, lf, nb, cx, hx, lr
%lp -> jt, bk
%vj -> ps
broadcaster -> km, lr, xh, rf
%dj -> pt, gc
%cg -> vd, hx
&ln -> tg
%fl -> pt, sk
%lm -> tr, bk
%lr -> vd, vg
&pt -> vq, rf, cm, jx, rg
%cx -> gp
%gp -> vd, sj
&db -> tg
%st -> vd
%jt -> bk
%jh -> lm, bk
%xf -> bd, tp
%gc -> cm, pt
&tp -> dr, km, kg, db, vj, qr
%ps -> xf, tp
%rf -> pt, dj
%lf -> nb
%bd -> tp, gg
%dk -> tp, vj
%mn -> jh, bk
&tg -> rx
%ql -> bk, zx
%tr -> bk, vp
%sk -> pt
%nb -> cg
%sb -> vd, cx
%qr -> dk
%xh -> bk, ql
%rg -> sd
%hx -> sb
%sd -> pt, jx
%gr -> bk, mn
%gg -> tp
%zx -> rq
&bk -> xh, ln, zx
%rm -> st, vd
%hq -> fl, pt
&vq -> tg
%cm -> rg
&tf -> tg
%mv -> pt, hq|} in 

let rx_parent = "tg" in 
let tg_index = find_index rx_parent nodes in 
let tg_parents = match (nodes --> tg_index).state with 
| Conjunction (_, _, parents) -> IntMap.to_list parents |> List.map fst
| _ -> failwith "oop"
in

let rec gcd a b =
  match b with
  | 0 -> a
  | _ -> gcd b (a mod b)
in

let lcm a b = a * b / gcd a b in 
let cts = List.map (find_high nodes) tg_parents in 

let res = match cts with 
| first::rest -> List.fold_left lcm first rest
| _ -> failwith "wow"
in

nodes |> print_nodes;
Printf.printf "Final count: %d\n" res