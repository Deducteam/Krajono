(* elpi: embedded lambda prolog interpreter                                  *)
(* copyright: 2014 - Enrico Tassi <enrico.tassi@inria.fr>                    *)
(* license: GNU Lesser General Public License Version 2.1                    *)
(* ------------------------------------------------------------------------- *)

(* Immutable array with basic slicing operations *)
module type S = sig
type 'a t

val init : int -> (int -> 'a) -> 'a t
val of_array : 'a array -> 'a t

val get : int -> 'a t -> 'a
val len : 'a t -> int
val sub : int -> int -> 'a t -> 'a t
val tl : 'a t -> 'a t
val hd : 'a t -> 'a

val map : ('a -> 'a) -> 'a t -> 'a t
val mapi : (int -> 'a -> 'a) -> 'a t -> 'a t

val fold_map : ('a -> 'b -> 'a * 'b) -> 'a t -> 'b -> 'a t * 'b

val fold : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b

val fold2 : ('a -> 'c -> 'b -> 'b) -> 'a t -> 'c t -> 'b -> 'b

val for_all : ('a -> bool) -> 'a t -> bool
val for_alli : (int -> 'a -> bool) -> 'a t -> bool

val for_all2 : ('a -> 'b -> bool) -> 'a t -> 'b t -> bool

val filter : ('a -> bool) -> 'a t -> 'a t

val to_list : 'a t -> 'a list
val of_list : 'a list -> 'a t
end

include S
