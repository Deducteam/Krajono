module refiner.

/************************* helpers ************************/

/* is_flex T :- not (not (dummy1__ = T)). */
is_flex T :- T is dummy1__.

is_same_flex M N :-
  is_flex M, is_flex N, not(dummy1__ = M, dummy2__ = N).

type prt string -> A -> o.
prt S T :- 
    print S, term_to_string T TMP, print TMP, print "\n".

type spy o -> o.
spy G :- prt "< " G, (G, prt "> " G ; prt ">fail " G).

/************************* refiner ************************/

of M _ _ _ :-  is_flex M, !, prt "GAME OVER" M, not true.

of (ginst M T) T (ginst M T) nil :- is_flex M, !.
of (ginst M T) T2 M1 Ex1 :- !, of M T2 M1 Ex1, unify T2 T.

of hole (ginst T set) (ginst M (ginst T set)) [ goal M (ginst T set), goal T set ].

sigma_appl [] [] _ _ :- !.
sigma_appl [S1 X|Ss] [decl T S1|Ss1] X T :- sigma_appl Ss Ss1 X T.
sigma_appl (append L R) (append L1 R1) X T :- sigma_appl L L1 X T, sigma_appl R R1 X T.

of (lam S F) (prod S2 T) (lam S2 F2) (append Ex1 Ex3) :-
  of S SO S2 Ex1,
  unify SO set,
  pi x\ sigma Ex2\ of x S2 x nil => (
    of (F x) (T x) (F2 x) Ex2,
    sigma_appl Ex2 /*=*/ Ex3 x S2).

of (prod S F) set (prod S2 F2) (append Ex1 Ex3) :-
  of S SO S2 Ex1,
  unify SO set,
  pi x\ sigma Ex2\ of x S2 x nil => (
    of (F x) (T x) (F2 x) Ex2,
    unify (T x) set,
    sigma_appl Ex2 /*=*/ Ex3 x S2).

of (app M1 N1) Z (app M2 N2) (append (append Ex1 Ex2) Ex4) :-
    of M1 TM1 M2 Ex1,
    of N1 TN1 N2 Ex2,
    pi x\ sigma Ex3\
      of hole (_ x) (F x) Ex3,
      unify TM1 (prod TN1 F),
      sigma_appl Ex3 /*=*/ Ex4 x TN1,
      subst F N2 Z.

of (atom ID) T (atom ID) nil :- env ID T.

env zero (atom nat).
env succ (prod (atom nat) (x \ (atom nat))).
env plus (prod (atom nat) (x\ prod (atom nat) (y\ (atom nat)))).
env nat set.
env vect (prod (atom nat) (x\ set)).
env vnil (app (atom vect) (atom zero)).
env vcons (prod (atom nat) (n\ prod (app (atom vect) n) (w\ app (atom vect) (app (atom succ) n)))).

of set set set nil.

of (rec Rty N Base Step) Rty2 (rec Rty2 N2 Base2 Step2) (append (append Ex1 Ex2) (append Ex3 Ex4)) :-
  of Rty TRty Rty2 Ex1,
  unify TRty set,
  of N TN N2 Ex2,
  unify TN (atom nat),
  of Base TBase Base2 Ex3,
  unify TBase Rty2,
  of Step TStep Step2 Ex4,
  unify TStep (prod (atom nat) n\ prod Rty2 acc \ Rty2).

/* retype */
rof T TY :- of T TY _ _.

/************************* clean ************************/

% clean L M :- prt "" (clean L M), not true.
clean (ginst M T1) R :-
 % prt "?FLEXIBLE " M,
 is_flex M, !,
 % prt "!FLEXIBLE " M,
 clean T1 T2,
 R = ginst M T2.
clean (ginst M1 _) M2 :- !,
 % prt "!RIGID " M1,
 clean M1 M2.
clean (app M1 N1) (app M2 N2) :- !, clean M1 M2, clean N1 N2.
clean (lam T1 F1) (lam T2 F2) :- !, clean T1 T2, pi x\ clean (F1 x) (F2 x).
clean (prod T1 F1) (prod T2 F2) :- !, clean T1 T2, pi x\ clean (F1 x) (F2 x).
clean (rec A1 B1 C1 D1) (rec A2 B2 C2 D2) :-
  !, clean A1 A2, clean B1 B2, clean C1 C2, clean D1 D2.
clean T T :- !.

% clean_seq L M :- prt "" (clean_seq L M), not true.
clean_seq (decl S1 F1) (decl S2 F2) :- clean S1 S2, pi x\ clean_seq (F1 x) (F2 x).
clean_seq (goal V T1) (goal V T2) :- is_flex V, !, clean T1 T2.

% clean_sigma L _ :- prt "" (clean_sigma L nil), not true.
clean_sigma [] [].
clean_sigma [X|Xs] [Y|Ys] :- clean_seq X Y, !, clean_sigma Xs Ys.
clean_sigma [_|Xs] Ys :- clean_sigma Xs Ys.
clean_sigma (append [] L1) L2 :- clean_sigma L1 L2.
clean_sigma (append [X|Xs] L1) L2 :- clean_sigma [X | append Xs L1] L2.
clean_sigma (append (append L1 L2) L3) L :- clean_sigma (append L1 (append L2 L3)) L.

/************************* ho ************************/

% ho (lam _ Res) What TYWhat Where

% only mimic in (? ? = T) case
ho (lam LTY (x\T)) (ginst N _) LTY T :- is_flex N, !.
ho (lam LTY (x\T)) (app (ginst N _) _) LTY T :- is_flex N, !.

% proj rigid
ho (lam LTY (x\x)) T LTY T2 :- unify T T2.

% mimic on compound terms
ho (lam LTY (x\ app (L1 x) (R1 x))) T LTY (app L R) :- ho (lam _ L1) T LTY L, ho (lam __ R1) T LTY R.
ho (lam LTY (x\ rec (A1 x) (B1 x) (C1 x) (D1 x))) T LTY (rec A B C D) :-
 ho (lam _ A1) T LTY A, ho (lam _ B1) T LTY B, ho (lam _ C1) T LTY C, ho (lam _ D1) T LTY D.
ho (lam LTY (x\ lam (L1 x) (R1 x))) T LTY (lam L R) :-
 ho (lam _ L1) T LTY L, pi x\ ho (lam __ (R1 x)) T LTY (R x).
ho (lam LTY (x\ prod (L1 x) (R1 x))) T LTY (prod L R) :-
 ho (lam _ L1) T LTY L, pi x\ ho (lam __ (R1 x)) T LTY (R x).

% mimic on atomic terms
ho (lam LTY (x\set)) T LTY set.
ho (lam LTY (x\A)) T LTY A :- A = atom _.

/************************* copy ************************/

copy (ginst G GT) (ginst G GT1) :- is_flex G, !, copy GT GT1.
copy (ginst G GT) (ginst G1 GT1) :- copy G G1, copy GT GT1.
copy set set.
copy A A :- A = atom _.
copy (app A B) (app A1 B1) :- copy A A1, copy B B1.
copy (lam T F) (lam T1 F1) :- copy T T1, pi x\ copy (F x) (F1 x).
copy (prod T F) (prod T1 F1) :- copy T T1, pi x\ copy (F x) (F1 x).

subst Where What Out :- pi x\ copy x What => copy (Where x) Out.

/************************* unify ************************/

%unif A M N :- prt "" (unif A M N), not true.

unif _ M _ :-  is_flex M, !, prt "GAME OVER" M, not true.
unif _ _ M :-  is_flex M, !, prt "GAME OVER" M, not true.

/* M=M */
unif ff (ginst M TM) (ginst N TN) :- is_same_flex M N, !, unify TM TN.

/* ginst with rigid body */
unif ff (ginst M T) N,
unif ff N (ginst M T) :- not (is_flex M), !, unify N M.

/* flex=term */
unif ff (ginst M T) N,
unif ff N (ginst M T) :-
  is_flex M,
  !,
  rof N TN,
  unify TN T,
  M = N.

/* reflexive closure + heuristic for == */
/*unif ff _ T T :- !.*/
unif ff set set :- !.
unif ff A A :- A = atom _, !.

/* contextual closure + heuristic */
unif ff (app H A) (app K B) :- unify H K, unify A B.

/* contextual closure */
unif ff (lam S F) (lam T G) :-
  !,
  unify S T,
  pi x\ of x S x nil => unif ff x x =>
    unify (F x) (G x).

unif ff (prod S F) (prod T G) :-
  !,
  unify S T,
  pi x\ of x S x nil => unif ff x x =>
    unify (F x) (G x).

/* contextual closure + heuristic */
unif ff (rec A1 B1 C1 D1) (rec A2 B2 C2 D2) :-
  unify A1 A2, unify B1 B2, unify C1 C2, unify D1 D2.

/* ho unification */
unif _ (app (ginst H TH) M) X :-
  is_flex H, !, rof M TYM, ho H2 M TYM X, unify (ginst H TH) H2.

/* beta */
unif _ (app L M) X :-
  L = lam _ F,
  !,
  subst F M Y,
  unify Y X.

/* delta */
unif _ (atom ID) B :- body ID B.

/* delta */
body plus (lam (atom nat)
             (n\ (rec (prod (atom nat) (x\ (atom nat))) n
                    (lam (atom nat) (x\ x))
                    (lam (atom nat) m\ lam (prod (atom nat) (x\ (atom nat))) acc\
                       lam (atom nat) (n\ app (atom succ) (app acc n)))))) :- !.
/* iota */
unif _ (rec _ N _ _) _ :- is_flex N, !, fail.
unif _ (rec _ N B R) X :- N = atom zero, !, unify B X.
unif _ (rec T N B R) X :- N = app (atom succ) M, !, unify (app (app R M) (rec T M B R)) X.

/* symmetric */
unif ff A B :- unif tt B A.

unify A B :- unif ff A B.

test_unify A B TA2 A2 B2 Sig :-
  prt "---------> " (of A TA A1 Ex1),
  of A TA A1 Ex1,
  prt "---------> " (of B TB B1 Ex2),
  of B TB B1 Ex2,
  prt "========== " (unify TA TB),
  unify TA TB,
  prt "========== " (unify A1 B1),
  unify A1 B1,
  print "cleaning1\n",
  clean TA TA2,
  print "cleaning2\n",
  clean A1 A2,
  print "cleaning3\n",
  clean B1 B2,
  print "cleaning4\n",
  clean_sigma (append Ex1 Ex2) Sig.
