(* elpi: embedded lambda prolog interpreter                                  *)
(* copyright: 2014 - Enrico Tassi <enrico.tassi@inria.fr>                    *)
(* license: GNU Lesser General Public License Version 2.1                    *)
(* ------------------------------------------------------------------------- *)

let debug = ref false
let dverbose = ref false
let where_loc = ref ("",0,max_int)
module M = Map.Make(String)
let cur_step = ref M.empty
let level = ref 0
let filter = ref []
let fonly = ref []
let hot = ref false

let get_cur_step k = try M.find k !cur_step with Not_found -> 0

let condition k =
  !debug &&
    let loc, first_step, last_step = !where_loc in
    ((!hot && k <> loc) ||
       (k = loc &&
       let cur_step = get_cur_step k in
       hot := cur_step >= first_step && cur_step <= last_step;
       !hot))
    && (!fonly = [] || List.exists (fun p -> Str.string_match p k 0) !fonly)
    && not(List.exists (fun p -> Str.string_match p k 0) !filter)

let init ?(where="",0,max_int) ?(skip=[]) ?(only=[]) ?(verbose=false) b =
  cur_step := M.empty;
  debug := b;
  dverbose := verbose;
  filter := List.map Str.regexp skip;
  fonly := List.map Str.regexp only;
  where_loc := where

let incr_cur_step k =
  let n = get_cur_step k in
  cur_step := M.add k (n+1) !cur_step

let exit_queue = ref []
let enqueue n s = exit_queue := (n,s) :: !exit_queue
let dequeue = function
  | None -> ()
  | Some d ->
      let rec aux = function
      | (d', (tab,e,time)) :: tl when d' >= d ->
          Format.eprintf "%s}}}\n%s%s  (%.3fs)\n%!" tab tab e time;
          aux tl
      | l -> exit_queue := l in
      aux !exit_queue

let quit () = dequeue (Some 0)

let enter k ?depth msg =
  dequeue depth;
  incr level;
  incr_cur_step k;
  if condition k then begin
    let depth, depthstr = match depth with
      | Some n -> n, Printf.sprintf "<%d>" n
      | None -> 0, "" in
    let tab = String.make (!level+depth) ' ' in
    Format.eprintf "%s%s %d {{{%s@[<hov1> %a@]\n%!"
      tab k (get_cur_step k) depthstr (fun fmt () -> msg fmt) ();
  end

let print name f x = 
  if condition name then
    Format.eprintf "%s %s =@[<hov1> %a@]\n%!" (String.make !level ' ') name f x

let printers = ref []

exception Unknown
exception OK

let pr_exc = function
  | OK -> "ok"
  | e ->
     let rec aux = function
       | [] -> "error: " ^Printexc.to_string e
       | f :: fs ->
             try f e
             with Unknown -> aux fs in
     aux !printers
let pr_exn f = printers := f :: !printers

let exit k ?(depth=0) ?(e=OK) time =
  if condition k then begin
    let tab = String.make (!level + depth) ' ' in
    enqueue depth (tab,(pr_exc e),time)
  end;
  decr level

let parse_argv argv =
  let on = ref false in
  let where = ref ("run",0,0) in
  let verbose = ref false in
  let skip = ref [] in
  let only = ref [] in
  let rec aux = function
    | [] -> []
    | "-trace-v" :: rest -> verbose := true; aux rest
    | "-trace-at" :: fname :: start :: stop :: rest ->
         where := (fname, int_of_string start, int_of_string stop);
         aux rest
    | "-trace-on" :: rest -> on := true; aux rest
    | "-trace-skip" :: expr :: rest ->
         skip := expr :: !skip;
         aux rest
    | "-trace-only" :: expr :: rest ->
         only := expr :: !only;
         aux rest
    | x :: rest -> x :: aux rest in
  let rest = aux (Array.to_list argv) in
  init ~where:!where ~verbose:!verbose ~only:!only ~skip:!skip !on;
  Array.of_list rest


