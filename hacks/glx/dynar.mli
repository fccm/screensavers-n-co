(** Dynamic Array *)

type 'a t

val of_array : 'a array -> 'a t
(** conversion from standard array *)

val to_array : 'a t -> 'a array
(** conversion to standard array *)

val push : 'a t -> 'a -> unit
(** add a new element at the end of the dynamic array *)

val pushi : 'a t -> 'a -> int
(** same than [push] and return the index of the added element *)

