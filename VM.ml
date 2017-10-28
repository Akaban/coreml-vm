open Printf
module IS = InstructionSet

let heap_size = 20

module Env = Map.Make(String)
type env = value Env.t
and value =
  | Int of int
  | Unit
  | Closure of string * IS.block * env
 

type heap = { mutable address : int ; mutable mem : (int, value) Hashtbl.t} 

(* Ici, version immuable *)
(*
type thread_state = {
  code  : block;
  stack : value list;
  env   : env
}
*)

type thread_state = {
  mutable code  : IS.block;
  mutable stack : value list;
  mutable env   : env ;
  mutable heap  : heap
}

exception End_of_thread of thread_state
    

let step state threads =
  let fetch() =
    match state.code with
      | []   ->
	raise (End_of_thread state)
      | i::c ->
	state.code <- c;
	i
  in
  let push v =
    state.stack <- v::state.stack
  in
  let pop() =
    match state.stack with
      | [] -> assert false
      | v::s ->
	state.stack <- s;
	v
  in
  match fetch() with
    | IS.Int(n) ->
      push (Int n)
	
    | IS.Lookup(id) ->
      printf "lookup for %s\n" id ;
      let v =
	Env.find id state.env
      in
      push v
      
    | IS.Add ->
      let Int n1, Int n2 = pop(), pop() in
      push(Int(n1+n2))

    | IS.Sub ->
      let Int n1, Int n2 = pop(), pop() in
      push(Int(n1-n2))

    | IS.Mult ->
      let Int n1,Int n2 = pop(), pop() in
      push(Int(n1*n2))

    | IS.Let(id) ->
      let v = pop() in
      printf "add value %s to env\n" id ;
      state.env <- Env.add id v state.env;

    | IS.EndLet(id) ->
      state.env <- Env.remove id state.env

    | IS.MkClos(id, e) ->
        let cl = Closure(id, e, state.env) in
        push(cl)

    | IS.Apply ->
      let v = pop() in
      let Closure(id, e, env') = pop() in
      let new_cl = Closure("apply_closure", state.code, state.env) in
      push new_cl ;
      state.code <- e ;
      state.env <- Env.add id v env'

    | IS.Return ->
      let v = pop() in
      let Closure(id, e, env') = pop() in
      push(v);
      state.env <- env';
      state.code <- e
    | IS.Alloc -> 
        let heap_ptr = state.heap.address in
        push(Int(heap_ptr)) ;
        state.heap.address <- heap_ptr + 1 ;
        Hashtbl.add state.heap.mem heap_ptr (Int 0)
    | IS.Store ->
        let v, Int(ptr) = pop(), pop() in
        Hashtbl.replace state.heap.mem ptr v
    | IS.Load ->
        let Int(ptr) = pop() in
        let v = Hashtbl.find state.heap.mem ptr in
        push(v)
    | IS.Unit -> push Unit
    | IS.Dup ->
        let v = pop() in
        push(v) ; push(v)
    | IS.Drop -> let _ = pop() in ()
    | IS.Spawn ->
        let v, Closure(id, e, env') = pop(), pop() in
        let new_thread = {code = e; stack=[]; env=(Env.add id v env'); heap=state.heap} in
        Queue.add new_thread threads


(*
let execute p : unit =
  let _ = (IS.print_prg p ; print_newline()) in 
  let b = {code=p; stack=[]; env=Env.empty;} in
  let rec exec state =
    step state; exec state
  in
  try
    exec b
  with End_of_thread(state) ->
    match state.stack with
    | Int(n)::s -> printf "%d\n" n
    | Closure(id, c, _)::s -> printf "closure\n"
*)

let rec print_clos id c = 
  printf "(fun %s -> " id;
  List.iter (fun inst ->
    match inst with
    | IS.Int(i) -> print_int i
    | IS.Lookup(id) -> printf " %s " id
    | IS.Add -> printf " + "
    | IS.Sub -> printf " - "
    | IS.Mult -> printf " * "
    | IS.Let (id) -> printf " %s " id
    | IS.MkClos(id, c) -> print_clos id c; printf ")"; 
    | _ -> ()
  ) c ; printf ")"

let execute p : unit =
  let print_from_stack = function
    | Int(n)::_ -> printf "%d\n" n
    | Closure(id, c, _)::_ -> print_clos id c ; print_newline() in
  let new_heap () = {address=0; mem=Hashtbl.create heap_size} in
  let _ = (print_string "Compiled prog = " ; Compile.print_prg p ; print_newline()) in
  let threads = Queue.create () in
  let fst_thread = {code=p ; env=Env.empty ; stack=[] ; heap=new_heap()} in
  Queue.add fst_thread threads ;
  while not (Queue.is_empty threads) do 
    let worker = Queue.peek threads in
    begin
    try
      begin
      while true do
        step worker threads ; done end
    with End_of_thread(state) ->
      let _ = Queue.take threads in (*remove thread*)
      print_from_stack state.stack
    end ; done ;;
