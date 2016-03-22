infixr --> 126. % type arrow

infixl '   255. % infix application
infixl ''  255. % infix System-F application
infixl &&  128. % and
infixl $$  127. % or
infixr ==> 126. % implication
infixr <=> 125. % iff
infix  #in 135. % membership
infix  <<= 130. % subseteq

/* Untrusted predicates called from the kernel:
 * next_object            next object to check
 * callback_proved        proof completed
 * next_tactic            next tactic to use
 * update_certificate     get new certificate after tactic application
 * end_of_proof           is the certificate/proof empty?
 * parse                  for pretty-printing messages
 */

/* Predicates exported from the kernel:
 * proves
 * check
 * ereterm
 */

{ /***** Trusted code base *******/

/* Trusted library predicates:
 * append
 * fold2_append
 * put_binds
 */

local thm, provable, def0, term, term', typ, loop, prove, check1, check1def,
 check1thm, reterm, reterm'.

proves T TY :- provable T TY.

typ bool.
typ ind.

/*term T TY :- $print (term T TY), fail.*/
term T TY :- $is_flex T, !.%, $delay (term T TY) [ T ].
term T TY :- term' T TY.
term' (lam A F) (A --> B) :- pi x\ term' x A => term (F x) B.
term' (F ' T) B :- term F (A --> B), term T A.
term' eq (A --> A --> bool).

/* like term, but on terms that are already known to be well-typed */
reterm T TY :- $is_flex T, !.%, $delay (reterm T TY) [ T ].
reterm T TY :- reterm' T TY.
reterm' (lam A F) (A --> B) :- !, pi x\ (reterm' x A :- !) => reterm (F x) B.
reterm' (F ' T) B :- !, reterm F (A --> B).
reterm' T TY :- term' T TY.

ereterm T TY :- reterm T TY.

/*propagate [ (G1 ?- term (X @ L1) TY1) ] [ (G2 ?- term (X @ L2) TY2) ] NEW :-
 list_map L1 (x\ y\ (term x y ; y = xxx)) LTY1,
 list_map L2 (x\ y\ (term x y ; y = xxx)) LTY2,
 NEW = (TY1 = TY2, LTY1 = LTY2).*/

/*propagate [ (G ?- term (X @ L) TY) ] [ (G ?- term (X @ L) TY) ] true .*/

% thm : bounded tactic -> bounded sequent -> list (bounded sequent) -> o
thm C (seq Gamma G) _ :- debug, $print Gamma "|- " G " := " C, fail.

/* << HACKS FOR DEBUGGING */
term' p bool.
term' q bool.
term' f (bool --> bool).
term' (g X) bool :- term X bool.
reterm' (g X) bool.
term' c bool.

thm daemon (seq Gamma F) [].
/* >> HACKS FOR DEBUGGING */

thm r (seq Gamma (eq ' X ' X)) [].
thm (t Y) (seq Gamma (eq ' X ' Z))
 [ seq Gamma (eq ' X ' Y), seq Gamma (eq ' Y ' Z) ] :- reterm X A, term Y A.
thm (m P) (seq Gamma Q) [ seq Gamma (eq ' P ' Q), seq Gamma P ] :- term P bool.
thm b (seq Gamma (eq ' ((lam _ F) ' X) ' (F X))) [].
thm c (seq Gamma (eq ' (F ' X) ' (G ' Y)))
 [ seq Gamma (eq ' F ' G) , seq Gamma (eq ' X ' Y) ] :- reterm X A, reterm Y A.
thm k (seq Gamma (eq ' (lam A S) ' (lam A T)))
 [ bind A x \ seq Gamma (eq ' (S x) ' (T x)) ].
thm s (seq Gamma (eq ' P ' Q)) [ seq (P :: Gamma) Q, seq (Q :: Gamma) P ] :-
 reterm P bool.
thm (h IGN) (seq Gamma P) [] :- append IGN [ P | Gamma2 ] Gamma.

thm d (seq Gamma (eq ' C ' A)) [] :- def0 C A.
thm (th NAME) (seq _ G) [] :- provable NAME G.

thm (thenll TAC1 TACN) SEQ SEQS :-
 thm TAC1 SEQ NEW,
 deftacl TACN NEW TACL,
 fold2_append TACL NEW thm SEQS.

/*debprint _ (then _ _) :- !.
debprint _ (thenl _ _) :- !.
debprint O T :- $print O T.*/

thm TAC SEQ SEQS :-
 deftac TAC SEQ XTAC,
 /*debprint "<<" TAC,
 (*/ thm XTAC SEQ SEQS /*, debprint ">>" TAC
 ; debprint "XX" TAC, fail)*/.

thm (! TAC) SEQ SEQS :-
 thm TAC SEQ SEQS,
 !.

thm id SEQ [ SEQ ].

thm (wl Gamma1) (seq Gamma F) [ seq WGamma F ] :-
 append Gamma1 [ P | Gamma2 ] Gamma,
 append Gamma1 Gamma2 WGamma.

thm (bind A TAC) (bind A SEQ) NEWL :-
 pi x \ term' x A => thm (TAC x) (SEQ x) (NEW x), put_binds (NEW x) x A NEWL.

thm ww (bind A x \ SEQ) [ SEQ ].

/* debuggin only, remove it */
%thm A B C :- $print "FAILED " (thm A B C), fail.

% loop : list (bounded sequent) -> certificate -> o
%loop SEQS TACS :- $print "LOOP" (loop SEQS TACS), fail.
loop [] CERTIFICATE :- end_of_proof CERTIFICATE.
loop [ SEQ | OLD ] CERTIFICATE :-
 next_tactic [ SEQ | OLD ] CERTIFICATE ITAC,
 thm ITAC SEQ NEW,
 append NEW OLD SEQS,
 update_certificate CERTIFICATE ITAC NEW NEW_CERTIFICATE,
 loop SEQS NEW_CERTIFICATE.

prove G TACS :-
 (term G bool, ! ; $print "Bad statement:" G, fail),
 loop [ seq [] G ] TACS.

/* check1 I O
   checks the declaration I
   returns the new assumption O */
check1 (theorem NAME GOALTACTICS) HYPS :-
  not (provable NAME _),
  check1thm NAME GOALTACTICS HYPS.
check1 (new_basic_type TYPE REP ABS REPABS ABSREP P TACTICS) HYPS :-
  not (typ TYPE),
  not (term REP _),
  not (term ABS _),
  not (term ABSREP _),
  not (term REPABS _),
  term P (X --> bool),
  prove (exists '' _ ' P ) TACTICS,
  callback_proved existence_condition (exists '' _ ' P) TACTICS,
  REPTYP = (TYPE --> X),
  ABSTYP = (X --> TYPE),
  ABSREPTYP = (forall '' TYPE ' lam TYPE x \ eq ' (ABS ' (REP ' x)) ' x),
  REPABSTYP = (forall '' X ' lam X x \ eq ' (P ' x) ' (eq ' (REP ' (ABS ' x)) ' x)),
  $print new typ TYPE,
  parse PREPTYP REPTYP, $print REP ":" PREPTYP,
  parse PABSTYP ABSTYP, $print ABS ":" PABSTYP,
  parse PABSREPTYP ABSREPTYP, $print ABSREP ":" PABSREPTYP,
  parse PREPABSTYP REPABSTYP, $print REPABS ":" PREPABSTYP,
  !,
  HYPS =
   ( typ TYPE
   , term' REP REPTYP
   , term' ABS ABSTYP
   , provable ABSREP ABSREPTYP
   , provable REPABS REPABSTYP
   ).
check1 (def NAME TYPDEF) HYPS :- not (def0 NAME _), check1def NAME TYPDEF HYPS.

check1def NAME (pi I) (pi HYPS) :-
 pi x \ check1def (NAME '' x) (I x) (HYPS x).
check1def NAME (TYP,DEF) HYPS :-
 term DEF TYP,
 parse PTYP TYP, $print NAME ":" PTYP,
 parse PDEF DEF, $print NAME "=" PDEF,
 TYDECL = term' NAME TYP,
 DEFDECL = def0 NAME DEF,
 HYPS = (DEFDECL, TYDECL).

check1thm NAME (pi I) (pi HYPS) :- pi x \ check1thm NAME (I x) (HYPS x).
check1thm NAME (GOAL,TACTICS) (provable NAME GOAL) :-
  prove GOAL TACTICS,
  callback_proved NAME GOAL TACTICS,
  !.

check WHAT :-
 next_object WHAT C CONT,
 (C = stop, !, K = true ; check1 C H , K = (H => check CONT)),
 !, K.

}

/************ parsing and pretty-printing ********/
parse X Y :- $is_flex X, $is_flex Y, !, X = Y.
parse X (F ' G) :- $is_flex X, ($is_flex F ; $is_flex G), !, X = (F ' G).
parse X (F ' G ' H) :- $is_flex X, ($is_flex F ; $is_flex G ; $is_flex H), !,
 X = (F ' G ' H).
parse (! F2) (forall '' _ ' lam _ F1) :- !, pi x \ parse (F2 x) (F1 x).
parse (? F2) (exists '' _ ' lam _ F1) :- !, pi x \ parse (F2 x) (F1 x).
parse (F2 = G2) (eq ' F1 ' G1) :- !, parse F2 F1, parse G2 G1.
parse (F2 <=> G2) (eq ' F1 ' G1) :- !, parse F2 F1, parse G2 G1.
parse (F2 && G2) (and ' F1 ' G1) :- !, parse F2 F1, parse G2 G1.
parse (F2 $$ G2) (or ' F1 ' G1) :- !, parse F2 F1, parse G2 G1.
parse (F2 ==> G2) (impl ' F1 ' G1) :- !, parse F2 F1, parse G2 G1.
parse (X2 #in S2) (in '' _ ' X1 ' S1) :- !, parse X2 X1, parse S2 S1.
parse (U2 <<= V2) (subseteq '' _ ' U1 ' V1) :- !, parse U2 U1, parse V2 V1.
parse (F2 ' G2) (F1 ' G1) :- !, parse F2 F1, parse G2 G1.
parse (lam A F2) (lam A F1) :- !, pi x \ parse (F2 x) (F1 x).
parse A A.

/* safe_list_map that unifies the two lists if they are both flexible
   probably only useful for parsing/pretty-printing */
safe_list_map L1 _ L2 :- $is_flex L1, $is_flex L2, !, L1 = L2.
safe_list_map L1 F L2 :- list_map L1 F L2.

parsetac daemon daemon.
parsetac r r.
parsetac (t Y) (t PY) :- parse Y PY.
parsetac (m Y) (m PY) :- parse Y PY.
parsetac b b.
parsetac c c.
parsetac k k.
parsetac s s.
parsetac (h Gamma) (h PGamma) :- safe_list_map Gamma parse PGamma.
parsetac d d.
parsetac (th NAME) (th NAME).
parsetac (thenll TAC1 TACN) (thenll PTAC1 PTACN) :-
 parsetac TAC1 PTAC1, parsetac TACN PTACN.
parsetac (! TAC) (! PTAC) :- parsetac TAC PTAC.
parsetac id id.
parsetac (wl Gamma) (wl PGamma) :- safe_list_map Gamma parse PGamma.
parsetac (bind A TAC) (bind PA PTAC) :-
 parse A PA, pi x \ parsetac (TAC x) (PTAC x).
parsetac ww ww.

/************ interactive and non interactive loops ********/

parse_obj (theorem NAME PSTTAC) [theorem NAME STTAC] :- parse_thm PSTTAC STTAC.
parse_obj (new_basic_type TYPE REP ABS REPABS ABSREP PP TACTICS)
 [new_basic_type TYPE REP ABS REPABS ABSREP P (false,TACTICS)] :- parse PP P.
parse_obj (new_basic_type TYPE REP ABS REPABS ABSREP PP)
 [new_basic_type TYPE REP ABS REPABS ABSREP P (true,[_])] :- parse PP P.
parse_obj (def NAME PTYBO) [def NAME TYBO] :- parse_def PTYBO TYBO.
parse_obj (inductive_def PRED PREDF PREDF_MON PRED_I PRED_E0 PRED_E K) EXP :-
 inductive_def_pkg PRED PREDF PREDF_MON PRED_I PRED_E0 PRED_E K EXP.

parse_def (pi I) (pi O) :- pi x \ parse_def (I x) (O x).
parse_def (TY,PB) (TY,B) :- parse PB B.

parse_thm (pi I) (pi O) :- pi x \ parse_thm (I x) (O x).
parse_thm (PST,TAC) (ST,(false,TAC)) :- !, parse PST ST.
parse_thm PST (ST,(true,[X])) :- parse PST ST.

next_object [ C | NEXT ] CT CONTNEXT :-
  parse_obj C [ CT | CONT ], append CONT NEXT CONTNEXT.
next_object [] C CONT :- 
 $print "Welcome to HOL extra-light",
 toplevel_loop [ C | CONT ].
next_object toplevel C CONT :- toplevel_loop [ C | CONT ].

read_cmd H :-
 $print "Enter a command or \"stop\"",
 read H,
 !.
read_cmd H :- read_cmd H.

toplevel_loop G :-
 read_cmd H,
 ( H = stop, !, G = [stop]
 ; parse_obj H PH, !, (G = PH ; $print "error", toplevel_loop G)
 ; $print "bad command", toplevel_loop G ).

callback_proved NAME GOAL (false,_) :- parse PGOAL GOAL, $print NAME ":" PGOAL.
callback_proved NAME G (true, [ TAC ]) :-
 canonical TAC CANONICALTAC,
 parse PG G,
 $print (theorem NAME (PG , [ CANONICALTAC ] )).

end_of_proof (true, []) :- $print "proof completed".
end_of_proof (false, []).

next_tactic0 [ SEQ | OLD ] (true, [ _ | _ ]) ITAC :-
 $print,
 list_iter_rev [ SEQ | OLD ] print_sequent,
 read_in_context SEQ ITAC BACKTRACK,
 BACKTRACK.
next_tactic0 SEQS (true, CERT) ITAC :-
 $print "error",
 next_tactic SEQS (true, CERT) ITAC.
next_tactic0 [ SEQ | OLD ] (false, [ TAC | _ ]) TAC.
next_tactic0 _ (false, _) ITAC :-
 $print "aborted",
 halt.

next_tactic SEQS CERT TAC :- next_tactic0 SEQS CERT PTAC, parsetac PTAC TAC.

update_certificate (true, [ TAC | OTHER_TACS ]) ITAC NEW (true, TACS) :-
 mk_script ITAC NEW NEW_TACS TAC,
 append NEW_TACS OTHER_TACS TACS.
update_certificate (false, [ _ | OTHER_TACS ]) _ _ (false, OTHER_TACS).

mk_script (bind A T) NEW NEW_TACS (bind A T2) :- !,
 pi x \
  put_binds (NEW2 x) x A NEW,
  mk_script (T x) (NEW2 x) (NEWT x) (T2 x),
  put_binds (NEWT x) x A NEW_TACS.
mk_script ITAC NEW NEW_TACS (thenl ITAC NEW_TACS) :-
 mk_list_of_bounded_fresh NEW NEW_TACS.

read_in_context (bind A K) (bind A TAC) BACKTRACK :-
 pi x \ term' x A => read_in_context (K x) (TAC x) BACKTRACK.
read_in_context (seq A B) TAC BACKTRACK :-
 read TAC, (TAC = backtrack, !, BACKTRACK = (!, fail) ; BACKTRACK = true).

print_sequent (seq Gamma G) :-
 $print,
 list_iter_rev Gamma (x \ sigma PX \ parse PX x, $print PX),
 $print "|------------------",
 parse PG G, $print PG.
print_sequent (bind A F) :- pi x \ print_sequent (F x).

/* turns thenl into then */
canonical (bind A T1) (bind A T2) :- !,
 pi x \ canonical (T1 x) (T2 x).
canonical (thenl T L) OTAC :- !,
 list_map L canonical L2,
 (mk_constant_list L2 S L2, !,
  (S = [], !, OTAC = T ; OTAC = then T S)
 ; OTAC = thenl T L2).
canonical T T.

/************ inductive_def package ********/
build_predicate P X [ (_,K) ] R :-
 process_constructor P X K R.
build_predicate P X [ (_,K) | REST ] (or ' Q ' R) :-
 process_constructor P X K Q,
 build_predicate P X REST R.

process_constructor P X (forall '' TY ' lam TY Q) (exists '' TY ' lam TY R) :-
 pi y \ process_constructor P X (Q y) (R y).
process_constructor P X (impl ' H ' K) (and ' H ' R) :-
 process_constructor P X K R.
process_constructor P X (P ' T) (eq ' X ' T).

inductive_def_pkg PRED PREDF PREDF_MONOTONE PRED_I PRED_E0 PRED_E L OUT :-
 (pi p \ list_map (L p)
  (x \ px \ sigma A \ sigma B \ sigma PB \ x = (A, B), parse B PB, px = (A, PB))
  (PL p)),
 (pi p \ pi x \ build_predicate p x (PL p) (P p x)),
 F = (lam _ p \ lam _ x \ P p x),
 !,
 OUT1 =
  [ def PREDF (_,F)
  , theorem PREDF_MONOTONE (monotone '' _ ' PREDF,
     [ then (conv (depth_tac (dd [PREDF]))) auto_monotone ])
  , def PRED (_,(fixpoint '' _ ' PREDF))
  , theorem PRED_I ((! x13 \ PREDF ' PRED ' x13 ==> PRED ' x13),
   [then forall_i
     (bind _ x13 \
       then (conv (rand_tac (rator_tac dd)))
        (then (conv (land_tac (rator_tac (rand_tac dd))))
          (then inv
            (then (cutth fixpoint_is_prefixpoint)
              (then (lforall PREDF)
                (thenl lapply [applyth PREDF_MONOTONE,
                  then
                   (g
                     (subseteq '' _ '
                       (PREDF ' (fixpoint '' _ ' PREDF)) '
                       (fixpoint '' _ ' PREDF)))
                   (then (conv (depth_tac (dd [subseteq])))
                     (then (conv (depth_tac (dd [in])))
                       (then (conv (depth_tac (dd [in])))(itaut 4))))]))))))])],
 list_map (PL PRED) (mk_intro_thm PRED_I) OUT2,
 append OUT1 OUT2 OUT12,
 OUT3 =
  [ theorem PRED_E0
    ((! x13 \
       (! x14 \ PREDF ' x13 ' x14 ==> x13 ' x14) ==>
        (! x14 \ PRED ' x14 ==> x13 ' x14)) ,
      [then forall_i
        (bind _ x13 \
          then (cutth fixpoint_subseteq_any_prefixpoint)
           (then (lforall PREDF)
             (then (lforall x13)
               (then (conv (depth_tac (dd [PRED])))
                 (then inv
                   (bind _ x14 \
                     then
                      (g
                        (impl ' (subseteq '' _ ' (PREDF ' x13) ' x13) '
                          (subseteq '' _ ' (fixpoint '' _ ' PREDF) ' x13)))
                      (then (conv (depth_tac (dd [subseteq])))
                        (then (conv (depth_tac (dd [subseteq])))
                          (then (conv (depth_tac (dd [in])))
                            (then (conv (depth_tac (dd [in])))
                              (then (conv (depth_tac (dd [in])))
                                (then (conv (depth_tac (dd [in])))
                                  (then
                                    (w
                                      (impl '
                                        (subseteq '' _ ' (PREDF ' x13) ' x13) '
                                        (subseteq '' _ '
                                          (fixpoint '' _ ' PREDF) ' x13)))
                                    (then inv
                                      (thenl lapply_last [h,
                                        then (lforall_last x14)
                                         (then lapply_last h)])))))))))))))))])
  ],
 append OUT12 OUT3 OUT.

mk_intro_thm PRED_I (NAME,ST)
 (theorem NAME (ST,
   [then inv (bind* (then (applyth PRED_I) (then (conv dd) (itaut 3))))])).

/************ library of basic data types ********/
/* bounded 'a ::= 'a | bind A (F : _ -> bounded 'a) */

% put_binds : list 'b -> 'a -> 'c -> list (bounded 'b) -> o
% put_binds [ f1,...,fn ] x t [ bind t x \ f1,...,bind t x fn ]
% binding all the xs that occur in f1,...,fn
%put_binds A B C D :- $print "PUT BINDS" (put_binds A B C D), fail.
put_binds [] _ _ [].
put_binds [ YX | YSX ] X A [ bind A Y | YYS ] :-
 YX = Y X, put_binds YSX X A YYS.
%put_binds A B C D :- $print "KO PUT BINDS" (put_binds A B C D), fail.

mk_bounded_fresh (bind _ F) (bind _ G) :- !, pi x\ mk_bounded_fresh (F x) (G x).
mk_bounded_fresh _ X.

mk_list_of_bounded_fresh [] [].
mk_list_of_bounded_fresh [S|L] [X|R] :-
 mk_bounded_fresh S X, mk_list_of_bounded_fresh L R.

/* list functions */

append [] L L.
append [ X | XS ] L [ X | RES ] :- append XS L RES.

fold2_append [] [] _ [].
fold2_append [ X | XS ] [ Y | YS ] F OUTS :-
 F X Y OUT, fold2_append XS YS F OUTS2, append OUT OUTS2 OUTS.

list_map [] _ [].
list_map [X|XS] F [Y|YS] :- F X Y, list_map XS F YS.

list_iter_rev [] _.
list_iter_rev [X|XS] F :- list_iter_rev XS F, F X.

mem [ X | _ ] X, !.
mem [ _ | XS ] X :- mem XS X.

mk_constant_list [] _ [].
mk_constant_list [_|L] X [X|R] :- mk_constant_list L X R.

bang P :- P, !.

/********** tacticals ********/

/*sigma ff \*/ deftac fail SEQ ff.

parsetac (constant_tacl TACL) (constant_tacl PTACL) :-
 list_map TACL parsetac PTACL.
deftacl (constant_tacl TACL) SEQS TACL.

parsetac (thenl TAC TACL) (thenl PTAC PTACL) :-
 parsetac TAC PTAC, list_map TACL parsetac PTACL. 
deftac (thenl TAC TACL) SEQ XTAC :-
 XTAC = thenll TAC (constant_tacl TACL).

parsetac (all_equals_list TAC) (all_equals_list PTAC) :- parsetac TAC PTAC.
deftacl (all_equals_list TAC2) SEQS TACL :-
 mk_constant_list SEQS TAC2 TACL.

parsetac (then TAC1 TAC2) (then PTAC1 PTAC2) :-
 parsetac TAC1 PTAC1, parsetac TAC2 PTAC2.
deftac (then TAC1 TAC2) SEQ XTAC :-
 XTAC = thenll TAC1 (all_equals_list TAC2).

parsetac (then! TAC1 TAC2) (then! PTAC1 PTAC2) :-
 parsetac TAC1 PTAC1, parsetac TAC2 PTAC2.
deftac (then! TAC1 TAC2) _ (then (! TAC1) TAC2).

parsetac (orelse TAC1 TAC2) (orelse PTAC1 PTAC2) :-
 parsetac TAC1 PTAC1, parsetac TAC2 PTAC2.
deftac (orelse TAC1 TAC2) SEQ XTAC :-
 XTAC = TAC1 ; XTAC = TAC2.

parsetac (orelse! TAC1 TAC2) (orelse! PTAC1 PTAC2) :-
 parsetac TAC1 PTAC1, parsetac TAC2 PTAC2.
deftac (orelse! TAC1 TAC2) _ (orelse (! TAC1) TAC2).

parsetac (bind* TAC) (bind* PTAC) :- parsetac TAC PTAC.
deftac (bind* TAC) SEQ (orelse! (bind _ x \ bind* TAC) TAC).

parsetac (repeat TAC) (repeat PTAC) :- parsetac TAC PTAC.
deftac (repeat TAC) SEQ XTAC :-
 ( XTAC = then TAC (repeat (bind* TAC))
 ; XTAC = id).

parsetac (repeat! TAC) (repeat! PTAC) :- parsetac TAC PTAC.
deftac (repeat! TAC) SEQ (orelse! (then! TAC (repeat! (bind* TAC))) id).

parsetac (printtac TAC) (printtac PTAC) :- parsetac TAC PTAC.
deftac (printtac TAC) SEQ TAC :-
 $print "SEQ" SEQ ":=" TAC.

parsetac (time TAC) (time PTAC) :- parsetac TAC PTAC.
deftac (time TAC) SEQ XTAC :-
 $gettimeofday B,
 XTAC = thenll TAC (time_after TAC B).

parsetac (time_after TAC B) (time_after PTAC B) :- parsetac TAC PTAC.
deftacl (time_after TAC B) SEQS TACL :-
 $gettimeofday A,
 D is A - B,
 mk_constant_list SEQS id TACL,
 $print "TIME SPENT " D "FOR" TAC.

/* For debugging only (?) For capturing metavariables */
parsetac (inspect (seq Gamma F) TAC) (inspect (seq PGamma PF) PTAC) :-
 list_map SEQ parse PSEQ, parse F PF, parsetac TAC PTAC.
deftac (inspect SEQ TAC) SEQ TAC.

/********** tactics ********/

parsetac (w G) (w PG) :- parse G PG.
deftac (w G) (seq Gamma _) (wl Gamma1) :-
 append Gamma1 [ G | _ ] Gamma.

parsetac h h.
deftac h SEQ (h L).

/*** eq ***/

parsetac sym sym.
deftac sym (seq Gamma (eq ' L ' R)) TAC :-
 TAC = thenl (m (eq ' R ' R)) [ thenl c [ thenl c [ r , id ] , r ] , r ].

parsetac eq_true_intro eq_true_intro.
deftac eq_true_intro (seq Gamma (eq ' P ' tt)) TAC :-
 TAC = thenl s [ th tt_intro, wl [] ].

/*** true ***/

/*** and ***/

parsetac conj conj.
deftac conj (seq Gamma (and ' P ' Q)) TAC :-
 TAC =
  then
   (then (conv dd)
     (then k (bind _ x \
       thenl c
        [ thenl c [ r, eq_true_intro ] ,
          eq_true_intro ])))
   ww.

/* Gamma  "|-"  q    --->   Gamma "|-" and ' p ' q*/
parsetac (andr P) (andr PP) :- parse P PP.
deftac (andr P) (seq Gamma Q) TAC :-
 TAC =
  (thenl (m ((lam _ f \ f ' P ' Q) ' (lam _ x \ lam _ y \ y)))
    [ then
       %(repeat (conv (depth_tac b))) ROBUS VERSION LINE BELOW
       (then (conv (land_tac b)) (then (conv (land_tac (rator_tac b))) (conv (land_tac b))))
      r
    , thenl (conv (rator_tac id))
       [ then (thenl (t (lam _ f \ f ' tt ' tt)) [ id, r ])
          (thenl (m (and ' P ' Q)) [ dd , id ])
       , then (repeat (conv (depth_tac b))) (th tt_intro) ]]).

/* (and ' p ' q) :: nil  "|-"  q */
parsetac andr andr.
deftac andr (seq Gamma Q) TAC :-
 mem Gamma (and ' P ' Q),
 TAC = then (andr P) h.

/* Gamma  "|-"  p    --->   Gamma "|-" and ' p ' q*/
parsetac (andl P) (andl PP) :- parse P PP.
deftac (andl Q) (seq Gamma P) TAC :-
 TAC =
  (thenl (m ((lam _ f \ f ' P ' Q) ' (lam _ x \ lam _ y \ x)))
    [ then
       %(repeat (conv (depth_tac b))) ROBUS VERSION LINE BELOW
       (then (conv (land_tac b)) (then (conv (land_tac (rator_tac b))) (conv (land_tac b))))
      r
    , thenl (conv (rator_tac id))
       [ then (thenl (t (lam _ f \ f ' tt ' tt)) [ id, r ])
          (thenl (m (and ' P ' Q)) [ dd , id ])
       , then (repeat (conv (depth_tac b))) (th tt_intro) ]]).

/* (and ' p ' q) :: nil  "|-"  p */
parsetac andl andl.
deftac andl (seq Gamma P) TAC :-
 mem Gamma (and ' P ' Q),
 TAC = then (andl Q) h.


/*** forall ***/

/* |- forall ' F  -->   |- F ' x */
parsetac forall_i forall_i.
deftac forall_i (seq Gamma (forall '' _ ' lam _ G)) TAC :-
 TAC = then (conv dd) (then k (bind _ x \ eq_true_intro)).

/* forall ' F |- F ' T */
parsetac forall_e forall_e.
deftac forall_e (seq Gamma GX) TAC :-
 mem Gamma (forall '' _ ' (lam _ G)), GX = G X,
 TAC = thenl (m ((lam _ G) ' X)) [ b, thenl (m ((lam _ z \ tt) ' X))
  [ thenl c [ then sym (thenl (m (forall '' _ ' lam _ G)) [dd,h ]), r ]
  , then (conv b) (th tt_intro) ] ].

/* forall ' F |- f  -->  F ' a, forall ' F |- f */
parsetac (lforall F A) (lforall PF PA) :- parse F PF, parse A PA.
deftac (lforall F A) (seq Gamma G) TAC :-
 TAC = thenl (m (impl ' (F A) ' G))
  [ thenl s [ then mp forall_e, then i h ] , then (w (forall '' _ ' lam _ F)) i ].

/* forall ' F |- f  -->  F ' a, forall ' F |- f */
parsetac (lforall A) (lforall PA) :- parse A PA.
deftac (lforall A) (seq Gamma G) (lforall F A) :-
 mem Gamma (forall '' _ ' lam _ F).

/* forall ' F |- f  -->  F ' a, forall ' F |- f */
parsetac lforall lforall.
deftac lforall (seq Gamma G) (lforall A).

/* forall ' F |- f  -->  F ' a, forall ' F |- f */
parsetac (lforall_last A) (lforall_last PA) :- parse A PA.
deftac (lforall_last A) (seq ((forall '' _ ' lam _ F)::Gamma) G) (lforall F A).

/*** false ***/
              
/*** impl ***/

/* |- p=>q  -->  p |- q */
parsetac i i.
deftac i (seq Gamma (impl ' P ' Q)) TAC :-
 TAC = then (conv dd) (thenl s [ andl, thenl conj [ h [], id ]]).

/* p=>q |- q  -->  |- p */
parsetac (mp P) (mp PP) :- parse P PP.
deftac (mp P) (seq Gamma Q) TAC :-
 TAC = then (andr P) (thenl (m P) [ then sym (thenl (m (impl ' P ' Q)) [ dd , h ]) , id ]).

/* p=>q |- q  -->  |- p */
parsetac mp mp.
deftac mp (seq Gamma Q) (mp P) :-
 mem Gamma (impl ' P ' Q).

/* |- q   -->   p |- q  and  |- p */
parsetac (cut P) (cut PP) :- parse P PP.
deftac (cut P) (seq Gamma Q) TAC :-
 TAC = then (andr P) (thenl (m P) [then sym (thenl (m (impl ' P ' Q)) [then (conv (land_tac dd)) r, i] ) , id]). 

/* |-q  --> p |- q   where the theorem T proves p */
parsetac (cutth P) (cutth PP) :- parse P PP.
deftac (cutth T) SEQ TAC :-
 proves T X,
 TAC = (thenl (cut X) [ id, th T ]).

/* applies the theorem T */
parsetac (applyth P) (applyth PP) :- parse P PP.
deftac (applyth T) SEQ (then (cutth T) apply_last).

/* impl p q, Gamma |- f   --->   /*impl q f*/ Gamma |- p  ,  q, Gamma |- f */
parsetac (lapply P Q) (lapply PP PQ) :- parse P PP, parse Q PQ.
deftac (lapply P Q) (seq Gamma F) TAC :-
 TAC =
  thenl (m (impl ' Q ' F)) [ thenl s [ then (mp Q) (then (w (impl ' Q ' F)) (then (mp P) (w (impl ' P ' Q)))) , then i (h [A]) ] , then (w (impl ' P ' Q)) (then i id) ].

/* impl p q, Gamma |- f   --->   /*impl q f*/ Gamma |- p  ,  q, Gamma |- f */
parsetac lapply lapply.
deftac lapply (seq Gamma F) (lapply P Q) :-
 mem Gamma (impl ' P ' Q).

/* impl p q, Gamma |- f   --->   /*impl q f*/ Gamma |- p  ,  q, Gamma |- f */
parsetac lapply_last lapply_last.
deftac lapply_last (seq ((impl ' P ' Q)::Gamma) F) (lapply P Q).

/* p |- f ---> p |- p ==> f */
parsetac (g P) (g PP) :- parse P PP.
deftac (g P) (seq _ F) TAC :-
 TAC =
  (thenl (m (impl ' P ' F)) [thenl s [then mp h , then i h] , id ]).

/*** not ***/

/*** exists ***/

/**** apply, i.e. forall + impl ****/

parsetac (apply X) (apply PX) :- parse X PX.
deftac (apply X) SEQ h :- $is_flex X, !.
deftac (apply X) SEQ h.
deftac (apply (impl ' P ' Q)) SEQ TAC :-
 TAC = thenl (lapply P Q) [ id, apply_last ].
deftac (apply (forall '' _ ' lam _ G)) SEQ TAC :-
 TAC = then (lforall G X) apply_last.

parsetac apply_last apply_last.
deftac apply_last (seq (H::Gamma) F) (apply H).

parsetac apply apply.
deftac apply (seq Gamma F) (apply H) :-
 mem Gamma H.

/********** conversion(als) ***********/

strip_constant (I '' _) H :- !, strip_constant I H.
strip_constant H H.

/* expands definitions, even if applied to arguments */
parsetac (dd L) (dd L).
deftac (dd L) (seq _ (eq ' T ' X)) d :- strip_constant T H, bang (mem L H).
deftac (dd L) (seq _ (eq ' (D ' T) ' X))
 (thenl (t A) [thenl c [dd L , r], b]).

parsetac dd dd.
deftac dd _ (dd L).

parsetac beta_expand beta_expand.
deftac beta_expand (seq _ (eq ' (lam _ x \ F x) ' (lam _ x \ (lam _ F) ' x))) TAC :-
 TAC = then k (bind _ x \ then sym b).

/* folds a definition, even if applied to arguments */
/* BUG: it seems to fail with restriction errors in some cases */
parsetac f f.
deftac f SEQ (then sym dd).

parsetac (rand_tac C) (rand_tac PC) :- parsetac C PC.
deftac (rand_tac C) SEQ TAC :-
  TAC = thenl c [ r , C ].

parsetac (rator_tac C) (rator_tac PC) :- parsetac C PC.
deftac (rator_tac C) SEQ TAC :-
  TAC = thenl c [ C , r ].

parsetac (abs_tac C) (abs_tac PC) :- parsetac C PC.
deftac (abs_tac C) SEQ TAC :-
  TAC = then k (bind A x \ C).

parsetac (land_tac C) (land_tac PC) :- parsetac C PC.
deftac (land_tac C) SEQ TAC :-
  TAC = thenl c [ thenl c [ r, C ] , r ].

parsetac (sub_tac C) (sub_tac PC) :- parsetac C PC.
deftac (sub_tac C) SEQ TAC :-
  TAC = orelse (rand_tac C) (orelse (rator_tac C) (abs_tac C)).

parsetac (try TAC) (try PTAC) :- parsetac TAC PTAC.
deftac (try TAC) SEQ (orelse TAC id).

parsetac (depth_tac C) (depth_tac PC) :- parsetac C PC.
deftac (depth_tac C) SEQ TAC :-
  TAC = then (try C) (sub_tac (depth_tac C)).

parsetac (conv C) (conv PC) :- parsetac C PC.
deftac (conv C) (seq Gamma F) TAC :-
 TAC = thenl (m G) [ then sym C , id ].

/********** Automation ***********/
/* TODO:
 1) our lforall gets rid of the hypothesis (bad) */
/* left tries to reduce the search space via focusing */
parsetac left left.
deftac left (seq Gamma _) TAC :-
 mem Gamma (not ' F),
 TAC =
  (!
   (then (cutth not_e)
    (then (lforall_last F)
     (thenl lapply [ h, (w (not ' F)) ])))).
deftac left (seq Gamma _) TAC :-
 /* A bit long because we want to beta-reduce the produced hypothesis.
    Maybe this should be automatized somewhere else. */
 mem Gamma (exists '' _ ' F),
 TAC =
  (!
   (then (cutth exists_e)
    (then (lforall_last F)
     (thenl lapply [ h, then (w (exists '' _ ' F)) (then apply_last (then forall_i (bind _ x \ then (try (conv (land_tac b))) i))) ])))).
deftac left (seq Gamma H) TAC :-
 mem Gamma (or ' F ' G),
 TAC =
  (!
   (then (cutth or_e)
    (then (lforall_last F)
     (then (lforall_last G)
      (then (lforall_last H)
       (thenl lapply [ h, then (w (or ' F ' G)) (then apply_last i)])))))).
deftac left (seq Gamma H) TAC :-
 mem Gamma (and ' F ' G),
 TAC =
  (!
   (then (cutth and_e)
    (then (lforall_last F)
     (then (lforall_last G)
      (then (lforall_last H)
       (thenl lapply [ h, then (w (and ' F ' G)) (then apply_last (then i i))])))))).
deftac left (seq Gamma H) TAC :-
 mem Gamma (eq ' F ' G),
 ereterm F TY,
 not ($is_flex TY), TY = bool,
 TAC =
  (then (g (eq ' F ' G))
   (then (conv (land_tac (then (applyth eq_to_impl) h)))
     (then i (w (eq ' F ' G))))).

parsetac not_i not_i.
deftac not_i (seq _ (not ' _)) (applyth not_i).

parsetac inv inv.
deftac inv _ TAC :-
 TAC =
 (then!
  (repeat!
   (orelse! conj (orelse! forall_i (orelse! i (orelse! not_i s)))))
  (bind* (repeat! left))).

parsetac (sync N) (sync N).
deftac (sync N) (seq _ tt) (th tt_intro).
deftac (sync N) (seq Gamma _) (then (applyth ff_elim) h) :-
 mem Gamma ff.
deftac (sync N) (seq _ (or ' _ ' _))
 (orelse (then (applyth orr) (itaut N)) (then (applyth orl) (itaut N))).
deftac (sync N) (seq _ (exists '' _ ' _)) (then (applyth exists_i) (then (conv b) (itaut N2))) :-
 N2 is N - 2.

parsetac (itaut N) (itaut N).
deftac (itaut N) SEQ fail :- N =< 0, !.
deftac (itaut N) SEQ TAC :-
 %$print (itaut N) SEQ,
 N1 is N - 1,
 N2 is N - 2,
 TAC =
 (then! inv
  (bind*
   (orelse h
   (orelse (sync N)
   (orelse /* Hypothesis not moved to front */ (then lforall (itaut N2))
   (then lapply (itaut N1))))))).

/********** inductive predicates package ********/

parsetac monotone monotone.
deftac monotone (seq _ (impl ' X ' X)) (! (then i h)) :- !.
deftac monotone (seq [forall '' _ ' lam _ x \ impl ' (F ' x) ' (G ' x)] (impl ' (F ' T) ' (G ' T))) (! apply) :- !.
deftac monotone (seq _ (impl ' (and ' _ ' _) ' _)) TAC :-
 TAC = then (applyth and_monotone) monotone.
deftac monotone (seq _ (impl ' (or ' _ ' _) ' _)) TAC :-
 TAC = then (applyth or_monotone) monotone.
deftac monotone (seq _ (impl ' (impl ' _ ' _) ' _)) TAC :-
 TAC = then (applyth impl_monotone) monotone.
deftac monotone (seq _ (impl ' (not ' _) ' _)) TAC :-
 TAC = then (applyth not_monotone) monotone.
deftac monotone (seq _ (impl ' (forall '' _ ' lam _ _) ' _)) TAC :-
 TAC =
  then (conv (land_tac (rand_tac beta_expand)))
   (then (conv (rand_tac (rand_tac beta_expand)))
     (then (applyth forall_monotone) (then forall_i (bind _ x \
       then (conv (depth_tac b)) (then (conv (depth_tac b)) monotone))))).
deftac monotone (seq _ (impl ' (exists '' _ ' lam _ _) ' _)) TAC :-
 TAC =
  then (conv (land_tac (rand_tac beta_expand)))
   (then (conv (rand_tac (rand_tac beta_expand)))
     (then (applyth exists_monotone) (then forall_i (bind _ x \
       then (conv (depth_tac b)) (then (conv (depth_tac b)) monotone))))).

/* expands "monotone ' (lam _ f \ lam _ x \ X f x)" into
   "! x \ p ' x ==> q ' x |- X p y ==> X q y"
   and then calls the monotone tactic */
parsetac auto_monotone auto_monotone.
deftac auto_monotone _ TAC :-
 TAC =
  then (conv dd)
   (then forall_i (bind _ p \ (then forall_i (bind _ q \
     then (conv (land_tac dd))
      (then (conv (land_tac (depth_tac (dd [in]))))
        (then (conv (land_tac (depth_tac (dd [in]))))
          (then i
            (then (conv dd)
              (then forall_i (bind _ x \
                (then (conv (land_tac dd))
                  (then (conv (rand_tac dd))
                    (then (conv (land_tac (rator_tac b)))
                      (then (conv (land_tac b))
                        (then (conv (rand_tac (rator_tac b)))
                          (then (conv (rand_tac b))
                            monotone)))))))))))))))).

/********** inductive things

defined in terms of new_type_definition
let nat_induct, nat_recurs = define_type "nat = O | S nat"

val nat_induct : !P. P O /\ ....
val nat_recurs : !f0 f1. ?f. f O = f0 /\ ...

term O nat
term S (nat --> nat)

injectivity nat
distinctiness nat
cases nat

smart way to instantiate nat_recurs
let plus = new_recursive_definition nat_RECUR
 "!n. plus O n = n /\ (!m n. plus (S m) n = S (plus m n)"
val plus : (!n. plus 0 n = n) /\ ....

every time you introduce a type you need to prove a theorem:
 example, to define even numbers out of natural numbers you prove
 ?n. EVEN n

you obtain natural numbers from the axiom
INFINITY_AX: ?f : ind -> ind. ONE_ONE f /\ -ONTO f

e.g. IND_SUC_0_EXISTS
?(f: ind -> ind) z. (!x1 x2. (f x1 = f x2) = (x1 = x2)) /\ (!x. ~(f x = z))

then use new_definition with
IND_SUC = @(f: ind -> ind). ?z. (!x1 x2. (f x1 = f x2) = (x1 = x2)) /\ (!x. ~(f x = z))
IND_0 = @z:ind. (!x1 x2. (IND_SUC x1 = IND_SUC x2) = (x1 = x2)) /\ (!x. ~(IND_SUC x = z))

and then your define the natural numbers

in the kernel two types: bool and ind (the type of individuals)

INFINITY_AX, SELECT_AX (* axiom of choice *), ETA_AX

*******/

/********** the library ********/

main :-
 check
  [ /*********** Connectives and quantifiers ********/
    def tt (bool,((lam bool x \ x) = (lam bool x \ x)))
  , def forall (pi A \ ((A --> bool) --> bool),
     (lam (A --> bool) f \ f = (lam A g \ tt)))
  , def ff (bool,(! x \ x))
  , def and ((bool --> bool --> bool),
     (lam _ x \ lam _ y \ (lam (bool --> bool --> bool) f \ f ' x ' y) = (lam _ f \ f ' tt ' tt)))
  , def impl ((bool --> bool --> bool),(lam _ a \ lam _ b \ a && b <=> a))
  , def exists (pi A \ ((A --> bool) --> bool),
     (lam (A --> bool) f \ ! c \ (! a \ f ' a ==> c) ==> c))
  , def not ((bool --> bool),(lam _ x \ x ==> ff))
  , def or ((bool --> bool --> bool),
     (lam _ x \ lam _ y \ ! c \ (x ==> c) ==> (y ==> c) ==> c))
  , theorem tt_intro (tt,[then (conv dd) (then k (bind _ x12 \ r))])
  , theorem ff_elim ((! p \ ff ==> p),
    [then forall_i (bind bool x3\ then (conv (land_tac dd)) (then i forall_e))])
  , theorem sym ((! p \ ! q \ p = q ==> q = p),
    [then forall_i
     (bind bool x12 \
       then forall_i
        (bind bool x13 \
          then i (then sym h)))])
  , theorem not_e ((! p \ not ' p ==> p ==> ff),
     [then forall_i (bind bool x3 \ then (conv (land_tac dd)) (then i h))])
  , theorem conj ((! p \ ! q \ p ==> q ==> p && q),
    [then forall_i
     (bind bool x12 \
      then forall_i (bind bool x13 \ then i (then i (then conj h))))])
  , theorem andl ((! p \ ! q \ p && q ==> p),
    [then forall_i
     (bind bool x12 \
      then forall_i (bind bool x13 \ then i (then (andl x13) h)))])
  , theorem andr ((! p \ ! q \ p && q ==> q),
    [then forall_i
     (bind bool x12 \
      then forall_i (bind bool x13 \ then i (then (andr x12) h)))])
  , theorem and_e ((! p \ ! q \ ! c \ p && q ==> (p ==> q ==> c) ==> c),
     [then forall_i
       (bind bool x12 \
         then forall_i
          (bind bool x13 \
            then forall_i
             (bind bool x14 \ then i (then i (thenl apply [andl, andr])))))])
  , theorem not_i ((! p \ (p ==> ff) ==> not ' p),
     [then forall_i (bind bool x2 \ then i (then (conv dd) h))])
  , theorem orl ((! p \ ! q \ p ==> p $$ q),
      [then forall_i
        (bind bool x12 \
          then forall_i
           (bind bool x13 \
             then i
              (then (conv dd)
                (then forall_i (bind bool x14 \ then i (then i (then apply h)))))))])
  , theorem orr ((! p \ ! q \ q ==> p $$ q),
      [then forall_i
        (bind bool x12 \
          then forall_i
           (bind bool x13 \
             then i
              (then (conv dd)
                (then forall_i (bind bool x14 \ then i (then i (then apply h)))))))])
  , theorem or_e ((! p \ ! q \ ! c \ p $$ q ==> (p ==> c) ==> (q ==> c) ==> c),
     [then forall_i
       (bind bool x12 \
         then forall_i
          (bind bool x13 \
            then forall_i
             (bind bool x14 \ then (conv (land_tac dd)) (then i forall_e))))])
  , theorem exists_e (pi T \
     (! f \ (exists '' T ' f) ==> (! c \ (! x \ f ' x ==> c) ==> c)),
     [then forall_i (bind (T --> bool) x12 \ then (conv (land_tac dd)) (then i h))])
 , theorem exists_i (pi T \ (! f \ ! w \ f ' w ==> (exists '' T ' f)),
    [then forall_i
     (bind (T --> bool) x12 \
       then forall_i
        (bind T x13 \
          then i
           (then (conv dd)
             (then forall_i
               (bind bool x14 \ then i (then (lforall x13) (then apply h)))))))])
  , theorem eq_to_impl
     ((! x13 \ ! x14 \ (x13 = x14) = ((x13 ==> x14) && (x14 ==> x13))),
     [thenl inv [(bind bool x13 \ bind bool x14 \ then (conv (then sym h)) h),
       (bind bool x13 \ bind bool x14 \ then (conv h) h),
       (bind bool x13 \ bind bool x14 \ itaut 2),
       (bind bool x13 \ bind bool x14 \ itaut 2)]])
 /******************* Logic *****************/
 , theorem or_commutative ((! a \ ! b \ a $$ b <=> b $$ a),
   [itaut 1])
 , theorem or_ff ((! a \ a $$ ff <=> a),
   [itaut 1])
 , theorem or_tt ((! a \ a $$ tt <=> tt),
   [itaut 1])
 , theorem or_idempotent ((! a \ a $$ a <=> a),
   [itaut 1])
 , theorem or_associative ((! a \ ! b \ ! c \ a $$ (b $$ c) <=> (a $$ b) $$ c),
   [itaut 1])
 , theorem and_commutative ((! a \ ! b \ a && b <=> b && a),
   [itaut 1])
 , theorem and_tt ((! a \ a && tt <=> a),
   [itaut 1])
 , theorem and_ff ((! a \ a && ff <=> ff),
   [itaut 1])
 , theorem and_idempotent ((! a \ a && a <=> a),
   [itaut 1])
 , theorem and_associative ((! a \ ! b \ ! c \ a && (b && c) <=> (a && b) && c),
   [itaut 1])
 , theorem and_or ((! a \ ! b \ ! c \ a && (b $$ c) <=> (a && b) $$ (a && c)),
   [itaut 1])
 , theorem or_and ((! a \ ! b \ ! c \ a $$ (b && c) <=> (a $$ b) && (a $$ c)),
   [itaut 1])
 , theorem ads_or_and ((! a \ ! b \ (a && b) $$ b <=> b),
   [itaut 1])
 , theorem ads_and_or ((! a \ ! b \ (a $$ b) && b <=> b),
   [itaut 1])
 , theorem not_or ((! a \ ! b \ not ' a && not ' b <=> not ' (a $$ b)),
   [itaut 2])
 , theorem not_and ((! a \ ! b \ not ' a $$ not ' b ==> not ' (a && b)),
   [itaut 2])
 , theorem not_not_not ((! p \ not ' (not ' (not ' p)) <=> not ' p),
   [itaut 3])
 , theorem impl_not_not ((! a \ ! b \ (a ==> b) ==> (not ' b ==> not ' a)),
   [itaut 3])
 , theorem eq_to_impl_f (((forall '' _ ' lam bool p \ ! q \
    (p <=> q) ==> p ==> q)),
    [itaut 2])
 , theorem eq_to_impl_b (((forall '' _ ' lam bool p \ ! q \
    (p <=> q) ==> q ==> p)),
    [itaut 2])
 /********** Monotonicity of logical connectives *********/
 , theorem and_monotone ((! a1 \ ! b1 \ ! a2 \ ! b2 \
    (a1 ==> b1) ==> (a2 ==> b2) ==> a1 && a2 ==> b1 && b2),
    [itaut 2])
 , theorem or_monotone ((! a1 \ ! b1 \ ! a2 \ ! b2 \
    (a1 ==> b1) ==> (a2 ==> b2) ==> a1 $$ a2 ==> b1 $$ b2),
    [itaut 2])
 , theorem impl_monotone ((! a1 \ ! b1 \ ! a2 \ ! b2 \
    (b1 ==> a1) ==> (a2 ==> b2) ==> (a1 ==> a2) ==> (b1 ==> b2)),
    [itaut 3])
 , theorem not_monotone ((! p \ ! q \ (p ==> q) ==> (not ' q) ==> (not ' p)),
    [itaut 3])
 , theorem forall_monotone ((! p \ ! q \
    (! x \ p ' x ==> q ' x) ==> (! x \ p ' x) ==> (! x \ q ' x)),
    [itaut 6])
 , theorem exists_monotone ((! p \ ! q \
    (! x \ p ' x ==> q ' x) ==> (? x \ p ' x) ==> (? x \ q ' x)),
    [itaut 6])

 /********** Knaster-Tarski theorem *********/
  , def in (pi A \ (A --> (A --> bool) --> bool),
     (lam A x \ lam (A --> bool) j \ j ' x))
  , def subseteq (pi A \ ((A --> bool) --> (A --> bool) --> bool),
     (lam (A --> bool) x \ lam (A --> bool) y \ ! z \ z #in x ==> z #in y))
  , theorem in_subseteq (pi A \ 
     (! s \ ! t \ ! x \ s <<= t ==> x #in s ==> x #in t),
     [then forall_i
       (bind (A --> bool) x9 \
         then forall_i
          (bind (A --> bool) x10 \
            then forall_i (bind A x11 \ then (conv (land_tac dd)) (itaut 4))))])
  , def monotone (pi A \ (((A --> bool) --> (A --> bool)) --> bool),
      (lam (_ A) f \ ! x \ ! y \ x <<= y ==> f ' x <<= f ' y))
  , def is_fixpoint (pi A \ (((A --> bool) --> (A --> bool)) --> ((A --> bool) --> bool)),
     (lam (_ A) f \ lam (_ A) x \ (f ' x) <<= x && x <<= (f ' x)))
  , def fixpoint (pi A \ (((A --> bool) --> (A --> bool)) --> (A --> bool)),
     (lam (_ A) f \ lam A a \ ! e \ f ' e <<= e ==> a #in e))
  , theorem fixpoint_subseteq_any_prefixpoint (pi A \
     (! f \ ! x\ f ' x <<= x ==> fixpoint '' A ' f <<= x),
     [then inv
       (bind ((A --> bool) --> (A --> bool)) x9 \
         (bind (A --> bool) x10 \
           then (conv (land_tac dd))
            (then (conv dd)
              (then forall_i
                (bind A x11 \
                  then (conv (land_tac dd))
                   (then (conv (land_tac b)) (itaut 4)))))))])
  , theorem fixpoint_subseteq_any_fixpoint (pi A \
     (! f \ ! x\ is_fixpoint '' A ' f ' x ==> fixpoint '' A ' f <<= x),
     [then forall_i
       (bind ((A --> bool) --> (A --> bool)) x9 \
         then forall_i
          (bind (A --> bool) x10 \
            then (conv (land_tac dd))
             (then (cutth fixpoint_subseteq_any_prefixpoint) (itaut 8))))])
  , theorem prefixpoint_to_prefixpoint (pi A \
     (! f \ ! x \ monotone '' A ' f ==> f ' x <<= x ==> f ' (f ' x) <<= f ' x),
    [then forall_i
      (bind ((A --> bool) --> (A --> bool)) x9 \
        then forall_i
         (bind (A --> bool) x10 \ then (conv (land_tac dd)) (itaut 6)))])
  , theorem fixpoint_is_prefixpoint (pi A \
     (! f \ monotone '' A ' f ==> f ' (fixpoint '' A ' f)<<= fixpoint '' A ' f),
     [then inv
       (bind ((A --> bool) --> (A --> bool)) x9 \
         then (conv dd)
          (then inv
            (bind A x10 \
              then (conv (depth_tac (dd [fixpoint])))
               (then (conv dd)
                 (then (conv b)
                   (then inv
                     (bind (A --> bool) x11 \
                       thenl (cut (fixpoint '' A ' x9 <<= x11))
                        [thenl
                          (cut (x9 ' (fixpoint '' A ' x9) <<= x9 ' x11))
                          [then (cutth in_subseteq)
                            (then (lforall_last (x9 ' x11))
                              (then (lforall_last x11)
                                (thenl apply_last [h,
                                  then (cutth in_subseteq) (itaut 10)]))),
                          thenl
                           (m (monotone '' A ' x9 ==> x9 ' (fixpoint '' A ' x9) <<= x9 ' x11))
                           [itaut 10, then (conv (land_tac dd)) (itaut 10)]],
                        then (applyth fixpoint_subseteq_any_prefixpoint) h])))))))])
  , theorem fixpoint_is_fixpoint (pi A \
    (! f \ monotone '' A ' f ==> is_fixpoint '' A ' f ' (fixpoint '' A ' f)),
    [then inv
      (bind ((A --> bool) --> (A --> bool)) x9 \
        then (conv (depth_tac (dd [is_fixpoint])))
         (thenl inv [then (applyth fixpoint_is_prefixpoint) h,
           then (applyth fixpoint_subseteq_any_prefixpoint)
            (then (g (monotone '' A ' x9))
              (then (conv (land_tac dd))
                (then inv
                  (then apply (then (applyth fixpoint_is_prefixpoint) h)))))]))])
 /******************* TESTS *****************/
 , theorem test_apply (p ==> (p ==> p ==> q) ==> q,
    [then i (then i (then apply h))])
 , theorem test_apply2 (p ==> (! x \ ! y \ x ==> x ==> y) ==> q,
    [then i (then i (then apply h))])
 , new_basic_type mybool myrep myabs myrepabs myabsrep
    (lam _ x \ ? p \ x = (p && p))
    [then (applyth exists_i)
      (then (conv b) (then (applyth exists_i) (then (conv b) r)))]
 , theorem test_itaut_1 (((? x \ g x) ==> ! x \ (! y \ g y ==> x) ==> x),
   [itaut 4])
 , theorem test_monotone1 (monotone '' _ ' (lam _ p \ lam _ x \ not ' (p ' x) ==> tt && p ' tt $$ p ' x),
   [ auto_monotone ])
 , theorem test_monotone2 (monotone '' _ ' (lam _ p \ lam _ x \ ? z \ not ' (p ' x) ==> tt && p ' tt $$ z),
   [ auto_monotone ])
 , theorem test_monotone3 (monotone '' _ ' (lam _ p \ lam _ x \ ! z \ ? y \ (not ' (p ' x) ==> z && p ' y $$ y)),
   [ auto_monotone ])
 , inductive_def pnn pnnF pnnF_monotone pnn_i pnn_e0 pnn_e (pnn \
     [ (pnn_tt, pnn ' tt)
     , (pnn_not, ! x \ pnn ' x ==> pnn ' (not ' x))])
 , theorem pnn_e
    ((! x13 \
       x13 ' tt && (! x14 \ x13 ' x14 ==> x13 ' (not ' x14)) ==>
        (! x14 \ pnn ' x14 ==> x13 ' x14)) ,
   [then forall_i
     (bind (bool --> bool) x13 \
       then (cutth pnn_e0)
        (then (lforall x13)
          (then i
            (thenl lapply
              [then (conv (depth_tac (dd [pnnF])))
                (then forall_i
                  (bind bool x14 \
                    then i
                     % from now on the proof is ad-hoc + fragile
                     (thenl left [then (conv (depth_tac h)) (itaut 1),
                       then left
                        (bind bool x15 \
                          then left (then (conv (depth_tac h)) (itaut 8)))]))),
              h]))))])
 , theorem pnn_has_two_values
    ((! x13 \ pnn ' x13 ==> x13 = tt $$ x13 = ff) ,
    % applying an elimination principle is hard: it should be automatized
    [then (cutth pnn_e)
      (then (lforall (lam bool x13 \ or ' (eq ' x13 ' tt) ' (eq ' x13 ' ff)))
        (thenl lapply
          [thenl conj [then (conv b) (itaut 1),
            then (repeat (conv (depth_tac b)))
             (then forall_i (bind bool x13 \ then i (then left (itaut 8))))],
          then inv
           (bind bool x13 \
             then (lforall x13)
              (thenl lapply [h,
                then
                 (g
                   ((lam bool x14 \ or ' (eq ' x14 ' tt) ' (eq ' x14 ' ff)) '
                     x13))
                 (then (repeat (conv (depth_tac b)))
                   (then
                     (w
                       ((lam bool x14 \ or ' (eq ' x14 ' tt) ' (eq ' x14 ' ff))
                         ' x13)) (then (w (pnn ' x13)) (itaut 2))))]))]))])
 , inductive_def two twoF twoF_monotone two_i two_e0 two_e (two \
     [ (two_tt, two ' tt)
     , (two_ff, two ' ff) ])
 , new_basic_type mybool2 myrep2 myabs2 myrepabs2 myabsrep2
    pnn
    [then (cutth pnn_tt) (then (applyth exists_i) h)]
 , theorem myrepabs2u
   ((! x13 \ pnn ' x13 ==> myrep2 ' (myabs2 ' x13) = x13),
   [then (cutth myrepabs2)
     (then forall_i
       (bind bool x13 \ then (lforall x13) (then (applyth eq_to_impl_f) h)))])
 , def mytt (mybool2,(myabs2 ' tt))
 , def mynot ((mybool2 --> mybool2),(lam _ x \ myabs2 ' (not ' (myrep2 ' x))))
 , theorem pnn_myrep2 ((! x13 \ pnn ' (myrep2 ' x13)) ,
    [then inv
     (bind mybool2 x13 \
       then (cutth myrepabs2)
        (then (lforall (myrep2 ' x13))
          (then (cutth myabsrep2)
            (then (lforall x13) (then (conv h) (thenl c [r, h]))))))]) 
/* , theorem mybool2_e
    (! p \ p ' mytt ==> (! y \ p ' y ==> p ' (mynot ' y)) ==>
      ! x \ p ' x)

   By npp_e + absrep + repabs
*/
 , theorem step0
    ((! x13 \ mynot ' (mynot ' (mynot ' x13)) = mynot ' x13) ,
     [then inv
      (bind mybool2 x13 \
        then (repeat (conv (depth_tac (dd [mynot]))))
         (thenl (conv (land_tac (rand_tac (rand_tac (applyth myrepabs2u)))))
          [then (cutth pnn_not)
            (then (lforall (myrep2 ' (myabs2 ' (not ' (myrep2 ' x13)))))
              (then (cutth pnn_myrep2)
                (then (lforall (myabs2 ' (not ' (myrep2 ' x13))))
                  (then apply h)))),
          thenl
           (conv
             (land_tac
               (rand_tac (rand_tac (rand_tac (applyth myrepabs2u))))))
           [then (cutth pnn_not)
             (then (lforall (myrep2 ' x13))
               (then (cutth pnn_myrep2)
                 (then (lforall x13) (then apply h)))),
           then (conv (land_tac (rand_tac (applyth not_not_not)))) r]]))])

/*
 , theorem step1 (! x \ x = mytt $$ x = mynot ' mytt $$ x = mynot ' (mynot ' mytt))

   By mybool2_e + mynot_mynot_mynot
*/
 ].

/* Status and dependencies of the tactics:
+dd:
+sym:
+eq_true_intro: (th tt_intro)
+forall_i: dd eq_true_intro
+conj: dd eq_true_intro
+andr: dd tt_intro
+andl: dd tt_intro
+forall_e: sym dd
+mp: andr sym dd
+i: dd andl conj
+cut: andr sym dd i
+cutth: cut
+lapply*: mp
+lforall*: mp forall_e
+apply*: lapply lforall
+applyth: cutth apply*

- f converional sometimes fails
- conv (depth_tac) diverges when applied to terms that contain
  metavariables
- repeat is not implemented using progress, that is not even there
*/

/*
-3) after main, if I do stop I see thousands of delayed goal that have
    never been resumed!

-2.5) in the proof for mybool, at the end I provide the
  witness (and X X) where X remains free (and it is not even pi-quantified).
  If bool was empty, then X could not exist. On the other hand, if X was
  empty, then there would be no need to provide the proof at all.
  In any case, the symptom for X remaining free at the end of a proof is
  one or more goals delayed on it. We never check for them and we have
  no way atm to do that. See bug -3)

-2.25) new_basic_type is only monomorphic at the moment. How to fix it?

-2) the test apply_2 is very slow: why?
    same for the witness for mybool

0) definitions must not be recursive; typing should capture it
   (but not if $delay is commented out...)

0.25) axioms are missing

0.75) Observation: so far our HOL-Light is intuitionistic.
 Keep it like that? Note: according to Wiedijk, new_basic_types makes it
 classical anyway (EM) provable

1) the need to use delay is a very good news. It justifies our
implementation and it easily allow to publish. We also need to add
the corresponding constraint propagation rules that implement the
unicity of typing meta-theorem. I.e.
  |- term X A,  |- term X B ==> |- A = B

The propagation rule is however harder. Consider:

  term x A |- term (X x) B,   |- term (X 0) C
  ===> A = nat, B = C


 We will discuss about it and we basically already have
 the code in the refiner.elpi file.

2) we need to fix the ELPI problems about handling of metavariables.
 I have already discussed with Enrico about them and he could have a
 shot at them. Namely:
 a) occur check + optimization to avoid it when possible
 b) unimplemented cases of restriction (IN PROGRESS)

3) once we let metavariables reach the goals, the current HOL-light 
 tactic implementation becomes too fragile. We should let the user 
 refer to hypotheses at least by number if not by name. But we better
 have a bidirectional successor/predecessor via $delay

4) we must implement type declarations and in particular inductive 
 types for HOL-light. It should also be a nice exercise in lambda-
 Prolog and the resulting code is likely to be easier than the 
 corresponding ML one. However, I never really had a look at the 
 mechanism used by HOL and we need to study it first

5) we could implement an automated theorem prover in lambdaProlog
 that works or is interfaced with the HOL-light code. There are
 complete provers like leanCOP 2.0 that are only 10 lines of code,
 but use some Prolog tricks.

6) we should do a small formalization, possibly developing a tactic,
 to prove that everything is working. For example, a decision procedure
 for rings or for linear inequations.

*/
