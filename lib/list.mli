val sum : int list -> int
(** Returns the sum of a list of ints *)

val take : int -> 'a list -> 'a list
(** Takes the first n elements from a list *)

val bifurcate : 'a list -> 'a list * 'a list
(** Splits a list in two halves *)

val dedup : 'a list -> 'a list
(** Removes duplicates from a list, leaving the first occurence *)

val union : 'a list -> 'a list -> 'a list
(** Calculates the union / intersection of two lists *)

val unions : 'a list list -> 'a list
(** Calculates the union / intersection of two or more lists *)

val groups : int -> 'a list -> 'a list list
(** Groups a list into a list of n-sized lists *)
