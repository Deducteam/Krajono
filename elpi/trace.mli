(* elpi: embedded lambda prolog interpreter                                  *)
(* copyright: 2014 - Enrico Tassi <enrico.tassi@inria.fr>                    *)
(* license: GNU Lesser General Public License Version 2.1                    *)
(* ------------------------------------------------------------------------- *)

val enter : string -> ?depth:int -> (Format.formatter -> unit) -> unit
val print : string -> (Format.formatter -> 'a -> unit) -> 'a -> unit
val exit : string -> ?depth:int -> ?e:exn -> float -> unit

exception Unknown
val pr_exn : (exn -> string) -> unit

val debug : bool ref
val dverbose : bool ref

val get_cur_step : string -> int

val parse_argv : string array -> string array
val quit : unit -> unit
