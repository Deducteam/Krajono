(***** library functions *****)
        
(*val filter_map : ('a -> 'b option) -> 'a list -> 'b list*)
let rec filter_map mapf =
 function
    [] -> []
  | hd::tl ->
     match mapf hd with
        None -> filter_map mapf tl
      | Some hd' -> hd'::filter_map mapf tl

(***** real code *****)

exception NotUnifiable of string Lazy.t;;

module type AtomT =
   sig        
        type t
        val pp : t -> string
        type bindings
        val pp_bindings : bindings -> string
        val empty_bindings : bindings
        (* raises NotUnifiable in case of failure *)
        val unify: bindings -> t -> t -> bindings
   end;;

module type Refreshable =
   sig        
        type r
        type refresher
        val empty_refresher : refresher
        val refresh : refresher -> r -> refresher * r
   end;;

module type RefreshableAtomT =
 sig include AtomT include Refreshable with type r := t end
;;

module AtomInt : RefreshableAtomT with type t = int =
   struct
        type t = int
        let pp n = "A" ^ string_of_int n

        type refresher = unit
        let empty_refresher = ()
        let refresh () n = (),n

        type bindings = unit
        let pp_bindings () = ""
        let empty_bindings = ()
        let unify () x y =
         if (x = y) then ()
         else
          raise (NotUnifiable
           (lazy (string_of_int x ^ " vs " ^ string_of_int y)))
   end;;

module AtomString : RefreshableAtomT with type t = string =
   struct
        type t = string
        let pp x = x

        type refresher = unit
        let empty_refresher = ()
        let refresh () n = (),n

        type bindings = unit
        let pp_bindings () = ""
        let empty_bindings = ()
        let unify () x y =
         if (x = y) then ()
         else
          raise (NotUnifiable
           (lazy (x ^ " vs " ^ y)))
   end;;

module type FormulaeT =
   sig
         type atomT
         type formula = 
            And of formula * formula
          | Or of formula * formula
          | True
          | Atom of atomT;;

         val pp : formula -> string
   end;;

module type RefreshableFormulaeT =
   sig
         include FormulaeT

         type clause = atomT * formula

         val refresh : clause -> clause
   end;;

module Formulae(Atom: AtomT) : FormulaeT with type atomT := Atom.t =
   struct
        type formula = 
                And of formula * formula
              | Or of formula * formula
              | True
              | Atom of Atom.t;;

        let rec pp =
         function
            And(f1,f2) -> "(" ^ pp f1 ^ "," ^ pp f2 ^ ")"
          | Or(f1,f2) -> "(" ^ pp f1 ^ ";" ^ pp f2 ^ ")"
          | True -> "true"
          | Atom a -> Atom.pp a
   end;;


module RefreshableFormulae(Atom: RefreshableAtomT) :
 RefreshableFormulaeT
  with type atomT := Atom.t
  and type formula = Formulae(Atom).formula
 =
   struct
        include Formulae(Atom)

        type clause = Atom.t * formula

        let refresh (hd,bo) =
         let l = Atom.empty_refresher in
         let l,hd = Atom.refresh l hd in
         let rec aux l =
          function
             And(f1,f2) ->
              let l,f1 = aux l f1 in
              let l,f2 = aux l f2 in
               l, And(f1,f2)
           | Or(f1,f2) ->
              let l,f1 = aux l f1 in
              let l,f2 = aux l f2 in
               l, Or(f1,f2)
           | True -> l, True
           | Atom a ->
              let l,a = Atom.refresh l a in
              l, Atom a in
          let _,bo = aux l bo in
          (hd,bo)
   end;;

module FormulaeInt = RefreshableFormulae(AtomInt);;
module FormualeString = RefreshableFormulae(AtomString);;

let test = FormulaeInt.And (FormulaeInt.Atom 0, FormulaeInt.Atom 1);;

(* --------------------------------------- *)

module type ProgramT =
   sig
        type t
        type atomT
        type formulaT
        type bindings
        val backchain: bindings -> atomT -> t -> 
                                        (bindings * formulaT) list
        val make: (atomT*formulaT) list -> t
   end

(* No indexing at all; a program is a list and retrieving the clauses
   from the program is O(n) where n is the size of the program.
   Matching is done using unification directly. *)
module Program(Atom: RefreshableAtomT) : ProgramT
    with type atomT := Atom.t
    and  type formulaT := RefreshableFormulae(Atom).formula
    and  type bindings := Atom.bindings =
   struct
        module Form = RefreshableFormulae(Atom)

        type t = (Atom.t*Form.formula) list
   
        (* backchain: bindings -> atomT -> 
                         (Atom.t*Form.formula) list -> 
                            (bindings * formulaT) list           *)
        let backchain binds a l =
          filter_map (fun clause -> 
            let atom,f = Form.refresh clause in
            try
                  let binds = Atom.unify binds atom a in 
                Some (binds,f)
            with NotUnifiable _ -> None) l                
        let make p = p;;
   end;;



module type HashableRefreshableAtomT =
 sig
  include RefreshableAtomT
  module IndexData : Hashtbl.HashedType 
  val index: t -> IndexData.t
 end
;;

(* One level indexing; a program is an hash-table indexed on the
   predicate that is the head of the clause. Unification is used
   eagerly on all the clauses with the good head. Retrieving the
   clauses is O(n) where n is the number of clauses with the
   good head. *)
module ProgramHash(Atom: HashableRefreshableAtomT) : ProgramT
    with type atomT := Atom.t
    and  type formulaT := RefreshableFormulae(Atom).formula
    and  type bindings := Atom.bindings =
   struct
        module Form = RefreshableFormulae(Atom)

        (* Atom.t -> (Atom.t*Form.formula) list *)
        module Hash = Hashtbl.Make(Atom.IndexData)
        type t = Form.clause Hash.t

        let rec filter_map mapf =
         function
            [] -> []
          | hd::tl ->
             match mapf hd with
                None -> filter_map mapf tl
              | Some hd' -> hd'::filter_map mapf tl
   
                  
      (* backchain: bindings -> atomT -> 
                         Form.formula Hash.t -> 
                            (bindings * formulaT) list           *)
        let backchain binds a h =
          let l = List.rev (Hash.find_all h (Atom.index a)) in
          filter_map (fun clause -> 
            let atom,f = Form.refresh clause in
            try
                  let binds = Atom.unify binds atom a in 
                Some (binds,f)
            with NotUnifiable _ -> None) 
            l                       
        
        let make p =
          let h = Hash.create 199 in
          List.iter
           (fun ((a,v) as clause) -> Hash.add h (Atom.index a) clause) p;
          h
   end;;

module type ApproximatableRefreshableAtomT =
 sig
  include RefreshableAtomT
  type approx
  val approx: t -> approx
  val matchp: approx -> approx -> bool
 end
;;

(* Two level inefficient indexing; a program is a list of clauses.
   Approximated matching is used to retrieve the candidates.
   Unification is then performed on the candidates.
   *** NOTE: this is probably redundant and more inefficient than
       just doing eager unification without approximated matching ***
   Retrieving the good clauses is O(n) where n is the size of the
   program. *)
module ProgramIndexL(Atom: ApproximatableRefreshableAtomT) : ProgramT
    with type atomT := Atom.t
    and  type formulaT := RefreshableFormulae(Atom).formula
    and  type bindings := Atom.bindings =
   struct
        module Form = RefreshableFormulae(Atom)

        (* triples (app,(a,f)) where app is the approximation of a *)
        type t = (Atom.approx * (Atom.t * Form.formula)) list

        (* backchain: bindings -> atomT -> 
                         Form.formula Hash.t -> 
                            (bindings * formulaT) list           *)
        let backchain binds a l =
          let app = Atom.approx a in
          let l = List.filter (fun (a',_) -> Atom.matchp app a') l in
          filter_map (fun (_,clause) ->
           let atom,f = Form.refresh clause in
           try
            let binds = Atom.unify binds atom a in 
            Some (binds,f)
           with NotUnifiable _ -> None
           ) l
        ;;
        
        let make =
          List.map (fun ((a,_) as clause) -> Atom.approx a,clause)
   end;;

(* ---------------------------------------- *)

module Run(Atom: RefreshableAtomT)(Prog: ProgramT with type atomT := Atom.t and type formulaT := RefreshableFormulae(Atom).formula and type bindings := Atom.bindings):
   sig
    type cont (* continuation *)
    val run: Prog.t -> RefreshableFormulae(Atom).formula ->
              (Atom.bindings * cont) option
    val next: cont -> (Atom.bindings * cont) option

    val main_loop: Prog.t -> RefreshableFormulae(Atom).formula -> unit
   end
   = struct 
        module F = RefreshableFormulae(Atom)

        (* A cont is just the program plus the or list,
           i.e. a list of bindings * head of and_list * tl of and_list *)
        type cont =
          Prog.t * (Atom.bindings * F.formula * F.formula list) list

        let run0 prog =
         (* aux binds andl orl f
           (binds,(f::andl))::orl) *)
         let rec aux binds andl orl =
          function
             F.True ->                  (* (True::andl)::orl *)
              (match andl with
                  [] -> Some (binds,orl)       (* (binds,[])::orl *)
                | hd::tl -> aux binds tl orl hd) (* (hd::tl)::orl *) 

           | F.Atom i ->                (*  (A::and)::orl) *)                       
               (match Prog.backchain binds i prog with              
                   [] ->
                    (match orl with
                        [] -> None
                      | (bnd,f,andl)::tl -> aux bnd andl tl f)
                 | (bnd,f)::tl ->
                     aux bnd andl
                      (List.append
                        (List.map (fun (bnd,f) -> bnd,f,andl) tl)
                        orl) f)
                    
           | F.And (f1, f2) ->             (* ((f1,f2)::andl)::orl *)
              aux binds (f2::andl) orl f1  (* (f1::(f2::andl))::orl *)

           | F.Or (f1, f2) ->              (* ((f1;f2)::andl)::orl) *)
              aux binds andl ((binds,f2,andl)::orl) f1
                                           (* (f1::andl)::(f2::andl)::orl *)
         in
          aux


        let run_next prog binds andl orl f =
         let time0 = Unix.gettimeofday() in
         let res =
          match run0 prog binds andl orl f with
             Some (binds,orl) -> Some (binds,(prog,orl))
           | None -> None in
         let time1 = Unix.gettimeofday() in
         prerr_endline ("Execution time: "^string_of_float(time1 -. time0));
         res

        let run prog f = run_next prog (Atom.empty_bindings) [] [] f

        let next (prog,orl) =
          match orl with
            [] -> None
          | (binds,f,andl)::orl -> run_next prog binds andl orl f

        let main_loop prog query =
         let rec aux =
          function
             None -> prerr_endline "Fail"
           | Some (l,cont) ->
              prerr_endline (Atom.pp_bindings l) ;
              prerr_endline "More? (Y/n)";
              if read_line() <> "n" then
               aux (next cont)
         in
          aux (run prog query)

     end

(*
open FormulaeInt;;

module ProgramInt = Program(AtomInt);;

let prog = ProgramInt.make
 [ 0, And (Atom 1, Atom 2)
 ; 1, Or (Atom 3, Atom 2)
 ; 3, Atom 4
 ; 2, Or (Atom 4, Atom 5)
 ; 5, True ]
;;

module RunInt = Run(AtomInt)(ProgramInt);; 

RunInt.main_loop prog (Atom 0);;
*)

(*------------------- TERM ------------------*)

module type VarT =
   sig
        type t
        val pp : t -> string
        val compare : t -> t -> int
        val eq : t -> t -> bool
        include Refreshable with type r := t
        val fresh : unit -> t
   end;;

module Variable : VarT = 
   struct
        type t = int
        let pp n = "X" ^ string_of_int n
        let compare = compare
        let eq = (=)

        type refresher = (int * int) list
        let empty_refresher = []

        let fresh,refresh =
         let maxvar = ref 0 in
         let fresh () = incr maxvar; !maxvar in
          fresh,
          (fun l n ->
            try l,List.assoc n l
            with Not_found ->
             let n' = fresh () in
             (n,n')::l,n')
   end;;

module type FuncT =
   sig
        type t
        val pp : t -> string
        val eq : t -> t -> bool
   end;;

module Func : FuncT with type t = int = 
   struct
        type t = int
        let pp n = "f" ^ string_of_int n
        let eq = (=)
   end;;

module FuncS : FuncT with type t = string = 
   struct
        type t = string
        let pp n = n
        let eq = (=)
   end;;

module type TermT =
   sig
            type varT
        type funcT
            type term = Var of varT | App of funcT * (term list)
        val pp : term -> string
   end;;

module Term(Var:VarT)(Func:FuncT) :
 TermT
  with type varT := Var.t
  and  type funcT := Func.t =
   struct
     type term = Var of Var.t | App of Func.t * (term list)

     let rec pp =
      function
         Var v -> Var.pp v
       | App(f,l) -> Func.pp f ^"("^ String.concat "," (List.map pp l) ^")"
   end

module type RefreshableTermT =
 sig include TermT include Refreshable with type r := term end
;;

module RefreshableTerm(Var:VarT)(Func:FuncT) :
 RefreshableTermT
  with type varT := Var.t
  and  type funcT := Func.t
  and  type term = Term(Var)(Func).term =
 struct
   include Term(Var)(Func)
   type refresher = Var.refresher
   let empty_refresher = Var.empty_refresher

   let rec refresh l =
    function
       Var v ->
        let l,v = Var.refresh l v in
        l, Var v
     | App(f,tl) ->
        let l,rev_tl =
          List.fold_left
           (fun (l,tl) t -> let l,t = refresh l t in (l,t::tl))
           (l,[]) tl
        in
        l, App(f,List.rev rev_tl)
 end

module type BindingsT =
   sig
        type varT
        type termT
        type bindings
        val pp_bindings : bindings -> string
        val empty_bindings: bindings
        (* bind sigma v t = sigma [v |-> t] *)
        val bind : bindings -> varT -> termT -> bindings
        (* lookup sigma v = Some t   iff  [v |-> t] \in sigma *)
        val lookup : bindings -> varT -> termT option 
   end

module Bindings(Vars: VarT)(Func: FuncT) : 
 BindingsT 
  with type varT := Vars.t
  and type termT := Term(Vars)(Func).term
  =
   struct
        module MapVars = Map.Make(Vars)
        module Terms = Term(Vars)(Func)
        type bindings = Terms.term MapVars.t

        let empty_bindings = MapVars.empty

        let lookup bind k =
         try Some (MapVars.find k bind)
         with Not_found -> None

        let bind bind k v = MapVars.add k v bind

        let pp_bindings bind =
         String.concat "\n"
          (MapVars.fold
            (fun k v s -> (Vars.pp k ^ " |-> " ^ Terms.pp v) :: s)
            bind [])
   end

module type UnifyT =
   sig
        type bindings
        type termT
        (* unify sub t1 t2 = sub'  iff  t1 sub' = t2 sub' and sub <= sub' *)
        val unify: bindings -> termT -> termT -> bindings         
   end;;

module Unify(Var: VarT)(Func: FuncT)(Bind: BindingsT with type termT := Term(Var)(Func).term and type varT := Var.t) :
 UnifyT
  with type bindings := Bind.bindings
  and type termT := Term(Var)(Func).term
=
   struct
        module T = Term(Var)(Func)

        let rec unify sub t1 t2 = match t1,t2 with
            (T.Var v1, T.Var v2) when Var.eq v1 v2 -> sub
            | (T.Var v1, _) ->
                (match Bind.lookup sub v1 with
                   None -> Bind.bind sub v1 t2
                 | Some t -> unify sub t t2)
          | (_, T.Var _) -> unify sub t2 t1
          | (T.App (f1,l1), T.App (f2,l2)) -> 
                if Func.eq f1 f2 && List.length l1 = List.length l2 then
                  List.fold_left2 unify sub l1 l2
                else
                  raise (NotUnifiable (lazy "Terms are not unifiable!"))
   end;;

module FOUnif = Unify(Variable)(Func)(Bindings(Variable)(Func))

module FOAtom(Var: VarT)(Func: FuncT)(Bind: BindingsT with type termT := Term(Var)(Func).term and type varT := Var.t) :
 RefreshableAtomT
  with type t = Term(Var)(Func).term
=
 struct
   include RefreshableTerm(Var)(Func)
   include Bind
   include Unify(Var)(Func)(Bind)
   type t = term
 end

module HashableFOAtom(Var: VarT)(Func: FuncT)(Bind: BindingsT with type termT := Term(Var)(Func).term and type varT := Var.t) :
 HashableRefreshableAtomT
  with type t = Term(Var)(Func).term
=
 struct
  include FOAtom(Var)(Func)(Bind)
  module IndexData =
   struct
    type t = Func.t
    let equal = Func.eq
    let hash = Hashtbl.hash
   end

  module TermFO = Term(Var)(Func)

  let index =
   function
      TermFO.App(f,_) -> f
    | TermFO.Var _ -> raise (Failure "Ill formed program")
 end

module ApproximatableFOAtom(Var: VarT)(Func: FuncT)(Bind: BindingsT with type termT := Term(Var)(Func).term and type varT := Var.t) :
 ApproximatableRefreshableAtomT
  with type t = Term(Var)(Func).term
=
 struct
  include FOAtom(Var)(Func)(Bind)

  type approx = Func.t * (Func.t option)

  module TermFO = Term(Var)(Func)

  let approx =
   function
      TermFO.App(f,[])
    | TermFO.App(f,TermFO.Var _::_) -> f,None
    | TermFO.App(f,TermFO.App(g,_)::_) -> f, Some g
    | TermFO.Var _ -> raise (Failure "Ill formed program")

  let matchp app1 app2 =
   match app1,app2 with
      (f1,None),(f2,_)
    | (f1,_),(f2,None)-> f1=f2
    | (f1,Some g1),(f2,Some g2) -> f1=f2 && g1=g2
 end

(*
module FOAtomImpl = FOAtom(Variable)(Func)(Bindings(Variable)(Func))
module FOProgram = Program(FOAtomImpl)
module RunFO = Run(FOAtomImpl)(FOProgram)
module FOTerm = Term(Variable)(Func)
module FOFormulae = RefreshableFormulae(FOAtomImpl)

open FOTerm;;
open FOFormulae;;

let p = 0
let q = 1
let two = 2
let x = Variable.fresh ()
let prog = FOProgram.make
 [ App(p,[Var x]), Atom(App(q,[Var x]))
 ; App(q,[App(two,[])]), True ]
;;

let y = Variable.fresh () in
let query = Atom (App(p,[Var y])) in
prerr_endline ("Testing " ^ FOFormulae.pp query);
RunFO.main_loop prog query
;;
*)


module FOAtomImpl = FOAtom(Variable)(FuncS)(Bindings(Variable)(FuncS))
module FOTerm = Term(Variable)(FuncS)
module FOFormulae = RefreshableFormulae(FOAtomImpl)

include FOFormulae
include FOTerm

type program = (FOAtomImpl.t * formula) list

(* List of implementations, i.e. quadruples
   msg: formula -> string
   load_and_run: program -> Program.t
   load_and_main_loop: program -> query -> unit *)
let implementations =
 let impl1 =
  (* RUN with two levels inefficient indexed engine *)
  let module FOAtomImplApprox = ApproximatableFOAtom(Variable)(FuncS)(Bindings(Variable)(FuncS)) in
  let module FOProgramApprox = ProgramIndexL(FOAtomImplApprox) in
  let module RunFOApprox = Run(FOAtomImplApprox)(FOProgramApprox) in
   (fun q -> "Testing with two level inefficient index "^FOFormulae.pp q),
   (fun (p : program) (q : formula) ->
     RunFOApprox.run (FOProgramApprox.make (Obj.magic p)) (Obj.magic q)
      = None),
   (fun (p : program) (q : formula) ->
     RunFOApprox.main_loop (FOProgramApprox.make (Obj.magic p))
      (Obj.magic q)) in

 let impl2 =
  (* RUN with indexed engine *)
  let module FOAtomImplHash = HashableFOAtom(Variable)(FuncS)(Bindings(Variable)(FuncS)) in
  let module FOProgramHash = ProgramHash(FOAtomImplHash) in
  let module RunFOHash = Run(FOAtomImplHash)(FOProgramHash) in
   (fun q -> "Testing with one level index hashtbl "^FOFormulae.pp q),
   (fun p q ->
     RunFOHash.run (FOProgramHash.make (Obj.magic p)) (Obj.magic q)
      = None),
   (fun p q ->
     RunFOHash.main_loop (FOProgramHash.make (Obj.magic p))
      (Obj.magic q)) in

 let impl3 =
  (* RUN with non indexed engine *)
  let module FOAtomImpl = FOAtom(Variable)(FuncS)(Bindings(Variable)(FuncS)) in
  let module FOProgram = Program(FOAtomImpl) in
  let module RunFO = Run(FOAtomImpl)(FOProgram) in
   (fun q -> "Testing with no index list "^FOFormulae.pp q),
   (fun p q ->
     RunFO.run (FOProgram.make (Obj.magic p)) (Obj.magic q)
      = None),
   (fun p q ->
     RunFO.main_loop (FOProgram.make (Obj.magic p))
      (Obj.magic q)) in

 [impl3;impl2;impl1]
;;
