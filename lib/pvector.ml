type 'a t = 
  | Null
  | Root of int * int * 'a t (* length, order, start node *)
  | Internal of 'a t * 'a t
  | Leaf of 'a * 'a

let (<<) l r = l lsl r
let (>>) l r = l lsr r

(* let's use ppx to make us be able to have arbitrary branching factor
   leaves will be factor-tuples. then make this whole thing a functor of branching factor *)

(* better v. of vec_from_generator where generator returns a 
   tuple of the value and the new context for next generator invoke *)

let vec_from_generator length generator = 
  if length = 0 then
    Root(0, 2, Null)
  else
    let depth = max 2 @@ (Float.ceil @@ Float.log2 (float_of_int length) |> int_of_float) in
    let total = 1 << depth in 
    let rec helper order pos = 
      if order = 1 then
        (if pos >= length then Null else 
          let left = generator pos in
          Leaf(left, if pos + 1 >= length then left else (generator @@ pos + 1)))
      else if order = total then 
        Root(length, total, helper (order >> 1) pos)
      else
        let sub_order = order >> 1 in 
        let do_right = pos + order < length in 
        Internal (helper sub_order pos, if not do_right then Null else helper sub_order @@ pos + order)
    in
    helper total 0

let make_vec length default = vec_from_generator length (fun _ -> default)
let empty = Root(0, 2, Null)

let len = function 
| Root (len, _, _) -> len
| _ -> failwith "invalid tree"

let order = function 
| Root (_, order, _) -> order
| _ -> failwith "invalid tree"

let rec tree_to_str = function 
| Null -> "Null"
| Leaf (_, _) -> "Leaf"
| Internal (a, b) -> "Internal(" ^ tree_to_str a ^ ", " ^ tree_to_str b ^ ")"
| Root (l, o, t) -> "Root(" ^ (string_of_int l) ^ ", " ^ (string_of_int o) ^ ", " ^ tree_to_str t ^ ")"

let is_null = function 
| Null -> true 
| _ -> false

let to_str str_of_el vec = 
  let l = len vec in 
  let rec h order pos = function
  | Null -> ""
  | Leaf (a, b) -> 
    if pos + 1 < l then (str_of_el a ^ ", " ^ str_of_el b) else str_of_el a
  | Internal (a, b) -> 
    if not (is_null b) then 
      h (order >> 1) pos a ^ ", " ^ h (order >> 1) (pos + order) b 
    else 
      h (order >> 1) pos a
  | Root (_, _, v) -> "[" ^ h (order >> 1) pos v ^ "]"
  in 
  h (order vec) 0 vec

let append el vec = 
  let target = len vec in 
  let rec helper t pos order = 
    match t with 
    | Null -> 
      if order = 1 then 
        Leaf(el, el) 
      else
        Internal(helper Null pos (order >> 1), Null)
    | Leaf (a, _) -> Leaf(a, el) (* make elements optional so we're clear? *)
    | Root (len, order, n) ->
       if order = len then (* full tree, so we need a new root one level up *)
        Root(len + 1, order << 1, helper (Internal(n, Null)) pos order)
       else
        Root(len + 1, order, helper n pos (order >> 1))
    | Internal (a, b) -> 
        let r_pos = pos + order in 
        if target >= r_pos then
          Internal(a, helper b r_pos (order >> 1))
        else 
          Internal(helper a pos (order >> 1), b)
  in 
  helper vec 0 @@ order vec

let at i vec = 
  let l = len vec in 
  if i >= l || i < 0 then 
    failwith "Index out of bounds"
  else 
  let rec helper order pos = function
    | Leaf(a, b) -> if i mod 2 = 0 then a else b
    | Internal(a, b) -> 
      let r_pos = pos + order in 
      if i >= r_pos then 
        helper (order >> 1) r_pos b
      else 
        helper (order >> 1) pos a
    | Root(_, _, t) -> helper (order >> 1) pos t
    | _ -> failwith "logical error"
  in
  helper (order vec) 0 vec

let set i el vec = 
  let l = len vec in 
  if i >= l || i < 0 then 
    failwith "Index out of bounds"
  else 
  let rec helper order pos = function
    | Leaf(a, b) -> if i mod 2 = 0 then Leaf(el, b) else Leaf(a, el)
    | Internal(a, b) -> 
      let r_pos = pos + order in
      if i >= r_pos then 
        Internal(a, helper (order >> 1) r_pos b)
      else
        Internal(helper (order >> 1) pos a, b)
    | Root(l, o, t) -> Root(l, o, helper (order >> 1) pos t)
    | _ -> failwith "logical error"
  in
  helper (order vec) 0 vec

let (<--) (vec, i) el = set i el vec
let (-->) vec i = at i vec

(* map is wrong, need pos tracking *)
let map fn vec = 
  let l = len vec in 
  let rec map pos order = function 
    | Null -> Null
    | Root (l, o, t) -> Root(l, o, map pos (order >> 1) t)
    | Internal (a, b) -> Internal(map pos (order >> 1) a, map (pos + order) (order >> 1) b)
    | Leaf (a, b) -> Leaf(fn a, if pos + 1 < l then fn b else b)
  in
  map 0 (order vec) vec

let mapi fn vec = 
  let l = len vec in 
  let rec map pos order = function 
    | Null -> Null
    | Root (l, o, t) -> Root(l, o, map pos (order >> 1) t)
    | Internal (a, b) -> Internal(map pos (order >> 1) a, map (pos + order) (order >> 1) b)
    | Leaf (a, b) -> Leaf(fn pos a, if pos + 1 < l then fn (pos + 1) b else b)
  in
  map 0 (order vec) vec

let any pred vec = 
  let l = len vec in 
  let rec h pos order = function 
  | Null -> false
  | Root(_, _, t) -> h pos (order >> 1) t
  | Internal(a, b) -> h pos (order >> 1) a || h (pos + order) (order >> 1) b
  | Leaf(a, b) -> pred a || pos + 1 < l && pred b
  in
  h 0 (order vec) vec

let anyi pred vec = 
  let l = len vec in 
  let rec h pos order = function 
  | Null -> false
  | Root(_, _, t) -> h pos (order >> 1) t
  | Internal(a, b) -> h pos (order >> 1) a || h (pos + order) (order >> 1) b
  | Leaf(a, b) -> pred pos a || pos + 1 < l && pred (pos + 1) b
  in
  h 0 (order vec) vec

let fold_left fn acc vec = 
  let l = len vec in 
  let rec fl pos order acc = function 
    | Null -> acc
    | Root (_, _, t) -> fl pos (order >> 1) acc t
    | Internal (a, b) -> fl (pos + order) (order >> 1) (fl pos (order >> 1) acc a) b
    | Leaf (a, b) -> if pos + 1 < l then fn (fn acc a) b else fn acc a
  in 
  fl 0 (order vec) acc vec

(* given a generator function A -> (A * B) option, and an initial context, return a vector
   generated by taking elements and stepping the context until generator returns None *)
let vec_from_sequence generator ctx = 
  let rec h ctx acc = 
    match generator ctx with 
    | Some (ctx, el) -> h ctx (append el acc)
    | None -> acc
  in
  h ctx empty

let of_list lst = vec_from_sequence (function | next::rest -> Some(rest, next) | [] -> None) lst
let of_string s = vec_from_generator (String.length s) (fun i -> String.get s i)

let rec count_slots tree = 
  match tree with 
  | Null -> 0
  | Root (_, _, c) -> count_slots c
  | Internal (a, b) -> count_slots a + count_slots b
  | Leaf _ -> 2