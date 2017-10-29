open Printf
module IS = InstructionSet

let heap_size = 20

module Env = Map.Make(String)
type env = value Env.t
and value =
  | Int of int
  | Unit
  | Closure of string * IS.block * env
  | Ptr of int

let rec print_closure =
  function 
    | Closure(id, c, env) ->
    begin
    let iter_fun = function
      | IS.Int(n) -> printf "%d" n
      | IS.Lookup(id) -> printf " %s " id
      | IS.Add -> printf " + "
      | IS.Sub -> printf " - "
      | IS.Mult -> printf " * "
      | IS.MkClos(id, c) -> print_closure (Closure(id, c, env)) ; printf ")"; 
      | IS.Let (id) -> printf " %s " id
      | _ -> assert false in 
      printf "(\\%s -> " id;
      List.iter (iter_fun) c ; printf ")" 
      end
    | _ -> () (*not a closure so silently fails*)

let print_value = function
  | Int n -> printf "%d\n" n
  | Unit -> ()
  | Ptr add -> printf "Ptr(%d)" add
  | Closure(_, _, _) as c -> print_closure c ; print_newline()

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
      let v = Env.find id state.env in
      push v
      
    | IS.Add ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1+n2))

    | IS.Sub ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1-n2))

    | IS.Mult ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1*n2)) 

    | IS.Let(id) ->
      let v = pop() in
      state.env <- Env.add id v state.env;

    | IS.EndLet(id) ->
      state.env <- Env.remove id state.env

    | IS.MkClos(id, e) ->
        let cl = Closure(id, e, state.env) in
        push(cl)
    | IS.If(e1, e2) ->
        printf "begin if\n" ;
        let Int v = pop() in
        if v==1 then
          state.code <- e1 @ state.code
        else 
          state.code <- e2 @ state.code

    (*| IS.While(id, b) ->
        let while_cl = Closure("while", state.code, state.env) in*)

    | IS.Apply ->
      let v = pop() in
      let Closure(id, e, env') = pop() in
      let new_cl = Closure("apply_closure", state.code, state.env) in
      push new_cl ;
      state.code <- e @ [IS.Return] ;
      state.env <- Env.add id v env'

    | IS.Return ->
      let v = pop() in
      let c = pop() in
      let Closure(id, e, env') = c in
      push(v);
      state.env <- env';
      state.code <- e
    | IS.Alloc -> 
        let heap_ptr = state.heap.address in
        push(Ptr(heap_ptr)) ;
        state.heap.address <- heap_ptr + 1 ;
        Hashtbl.add state.heap.mem heap_ptr (Int 0) (*value initialized at 0 (standard behavior)*)
    | IS.Store ->
        let v = pop() in
        let Ptr ptr = pop() in
        Hashtbl.replace state.heap.mem ptr v
    | IS.Load ->
        let Ptr(ptr) = pop() in
        let v = Hashtbl.find state.heap.mem ptr in
        push(v)
    | IS.Unit -> push Unit
    | IS.Dup ->
        let v = pop() in
        push(v) ; push(v)
    | IS.Drop -> let _ = pop() in ()
    | IS.Spawn ->
        let v = pop() in 
        let Closure(id, e, env') = pop() in
        let new_thread = {code = e; stack=[]; env=(Env.add id v env'); heap=state.heap} in
        Queue.add new_thread threads

let print_stack = List.iter (print_value)

let execute p : unit =
  let print_from_stack = function
    | p::_ -> print_value p
    | [] -> () in
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
