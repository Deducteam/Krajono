type comment = string

type var = string

type constname = string

type modname = string

type const = modname * constname

type sort = Type | Kind

type term =
| Var   of var
| Const of const
| Sort  of sort
| Prod  of var * term * term
| Lam   of var * term * term
| App   of term * term

let prods bs a = List.fold_right (fun (x, b) a -> Prod(x, b, a)) bs a
let lams bs m = List.fold_right (fun (x, b) m -> Lam(x, b, m)) bs m
let apps m ns = List.fold_left (fun m n -> App(m, n)) m ns
let app_bindings m bs =
  let xs = (fst (List.split bs)) in
  apps m (List.map (fun x -> Var x) xs)

type pattern =
| PVar of var
| PConst of const
| PLam of var * pattern * pattern
| PApp of pattern * pattern
| PGuard of term

let plams bs m = List.fold_right (fun (x, b) m -> PLam(x, PGuard b, m)) bs m
let papps m ns = List.fold_left (fun m n -> PApp(m, n)) m ns
let papp_bindings m bs =
  let xs = (fst (List.split bs)) in
  papps m (List.map (fun x -> PVar x) xs)

type context = (var * term) list

let app_context m context =
  (* Contexts are stored in reverse order. *)
  app_bindings m (List.rev context)

let papp_context m context =
  (* Contexts are stored in reverse order. *)
  papp_bindings m (List.rev context)

type command =
| Require of modname

type entry =
| StcDeclaration of constname * term
| DefDeclaration of constname * term
| Definition of constname * term option * term
| RewriteRule of context * pattern * term
| Command of command
| Comment of string

type signature = entry list
