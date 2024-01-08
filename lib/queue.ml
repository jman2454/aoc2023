type 'a t = 'a list * 'a list
let empty : 'a t = [], []

let is_empty ((f, _) : 'a t) = f = []

let enq x (f, b) =
  if f = [] then [x], []
  else f, x :: b

let front (f, _) = 
  List.hd f 

let deq (f, b) =
  match List.tl f with
  | [] -> List.rev b, []
  | t -> t, b