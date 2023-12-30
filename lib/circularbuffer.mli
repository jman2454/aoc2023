type 'a t = Node of 'a * 'a list * 'a list | Empty
val next : 'a t -> 'a t
val of_list : 'a list -> 'a t
val get_value : 'a t -> 'a
val iter : ('a -> unit) -> 'a t -> int -> unit
val map : ('a -> 'b) -> 'a t -> int -> 'b list