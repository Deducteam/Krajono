(** Pretty-printer for Dedukti **)
val print_pattern : Format.formatter -> Dedukti.pattern -> unit

val print_term : Format.formatter -> Dedukti.term -> unit

val print_entry : Format.formatter -> Dedukti.entry -> unit

val print_signature : Format.formatter -> Dedukti.signature -> unit
