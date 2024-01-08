type 'a t = 'a list * 'a list
val empty : 'a t
val is_empty : 'a t -> bool
val enq : 'a -> 'a list * 'a list -> 'a list * 'a list
val front : 'a list * 'b -> 'a
val deq : 'a list * 'a list -> 'a list * 'a list
