sig refiner.

kind term type.

/** PTS */
type app  term -> term -> term.
/* lam sty body */
type lam  term -> (term -> term) -> term.
type prod term -> (term -> term) -> term.
type set  term.

/** Arithmetics */
type nat term.
type zero term.
type succ term.
/* rec rty n base step:(n -> res_n -> rty) */
type rec  term -> term -> term -> (term -> term -> term) -> term.

/** Dependent type */
type vnil term.
type vcons term.
type vect term.

/** Constants with a body ??? */
type plus term.

/** untyped */
type hole term.

/* hack */
kind bool type.
type tt bool.
type ff bool.

type mem term -> list (list term) -> term -> o.
type find term -> list (list term) -> term -> o.


/** Program */
/* of term type term' */
type of   list (list term) -> term -> term -> term -> list (list term) -> o.
type unif bool -> list (list term) -> term -> term -> o.
type unify list (list term) -> term -> term -> o.
type rof list (list term) -> term -> term -> o.
type test_unify list (list term) -> term -> term -> term -> term -> list (list term) -> o.

type dummy1__, dummy2__ term.
type is_flex term -> o.
type is_same_flex term -> term -> o.
