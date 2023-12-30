
type 'a t = Node of 'a * 'a list * 'a list | Empty

let next a = 
  match a with 
  | Empty -> Empty 
  | Node (_, next_pos, l) ->
    (match next_pos with 
    | [] -> Node (List.hd l, List.tl l, l)
    | a::rest -> Node (a, rest, l))

let of_list list = 
  match list with 
  | [] -> Empty
  | a::rest -> Node (a, rest, list)

let get_value t = 
  match t with 
  | Node (v, _, _) -> v
  | Empty -> failwith "empty buffer!"

let iter f buf count = 
  let rec h t n = 
    match n with 
    | 0 -> ()
    | _ -> f (get_value t); h (next t) (n-1)
  in 
  h buf count

(* no reason ever to map over length, but allow it anyway - lazy *)
let map f buf count = 
  let rec h t n acc = 
    match n with 
    | 0 -> acc
    | _ -> h (next t) (n-1) (f (get_value t)::acc)
  in 
  h buf count []