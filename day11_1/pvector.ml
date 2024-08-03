type 'a t = 
  | Null
  | Root of int * int * 'a t (* length, depth, start node *)
  | Internal of 'a t * 'a t
  | Leaf of 'a * 'a

let make_vec length def = 
  let depth = Float.ceil @@ Float.log2 (float_of_int length) |> int_of_float in
  let total = 1 lsl depth in 
  let rec helper order pos = 
    if order = 1 then 
      (if pos >= length then Null else Leaf (def, def))
    else if order = total then 
      Root(length, depth, helper (order lsr 1) pos)
    else
      let sub_order = order lsr 1 in 
      Internal (helper sub_order pos, helper sub_order @@ pos + order)
  in
  helper total 0

(* let append vec el = 
  let rec helper t pos d = 
    match t with 
    | Root (len, d, t2) ->
       let kids = 1 lsl d in 
       if kids = len then 
       Root(i + 1, d, helper t2 pos d)
    | Internal (a, b) -> Internal(a, b)
    | _ -> failwith "bad"
  in 
  Null *)

let rec count tree = 
  match tree with 
  | Null -> 0
  | Root (_, _, c) -> count c
  | Internal (a, b) -> count a + count b
  | Leaf _ -> 2
(* log2 of length *)