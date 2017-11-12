open Printf
module IS = InstructionSet

let heap_size = 20

exception VMError of string
exception EmptyStack

module Env = Map.Make(String)
type env = value Env.t
and value =
  | Int of int
  | Unit
  | Closure of string * IS.block * env
  | Ptr of int

let rec closure_of_string =
  function 
    | Closure(id, c, env) ->
    begin
    let iter_fun = function
      | IS.MkClos(id, c) -> closure_of_string (Closure(id, c, env)) 
      | ins -> IS.string_of_is ins in
    (sprintf "(\\%s -> " id) ^
    (List.fold_right (fun x acc -> iter_fun x ^ acc) c "") ^ ")" 
      end
    | _ -> ""



let print_closure c = printf "%s\n" (closure_of_string c)

let string_of_value = function
  | Int n -> sprintf "%d" n
  | Unit -> "Unit"
  | Ptr add -> sprintf "Ptr(%d)" add
  | Closure(_, _, _) as c -> sprintf "closure " ^ (closure_of_string c) 



let print_value v = printf "%s\n" (string_of_value v)

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
exception Wait_of_thread  

let step state threads =
  let int_of_bool = function
    | true -> 1
    | false -> 0 in
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
      | [] -> raise EmptyStack
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
      let v1 = pop() in
      let v2 = pop() in
      begin
      match v1,v2 with
        | Int(n1), Int(n2) -> push(Int(n1+n2))
        | Ptr(n1), Ptr(n2) -> raise (VMError "TypeError: Additions between pointers are not allowed")
        | Int(n1), Ptr(n2)
        | Ptr(n1), Int(n2) -> printf "Warning: Pointer addition with integer\n" ; push(Ptr(n1+n2))
        end

    | IS.Sub ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1-n2))

    | IS.Div ->
        let Int n1 = pop() in
        let Int n2 = pop() in
        push(Int(n1/n2))

    | IS.Mult ->
      let Int n1 = pop() in
      let Int n2 = pop() in
      push(Int(n1*n2))

    | IS.Geq ->
        let Int n1 = pop() in
        let Int n2 = pop() in
        let v = n1 >= n2 in
        push(Int(int_of_bool(v)))

    | IS.Leq ->
        let Int n1 = pop() in
        let Int n2 = pop() in
        let v = n1 <= n2 in
        push(Int(int_of_bool(v)))
    | IS.Eq ->
        let Int n1 = pop() in
        let Int n2 = pop() in
        let v = n1 == n2 in
        push(Int(int_of_bool(v)))
    | IS.Lt ->
        let Int n1 = pop() in
        let Int n2 = pop() in
        let v = n1 < n2 in
        push(Int(int_of_bool(v)))
    | IS.Gt ->
        let Int n1 = pop() in
        let Int n2 = pop() in
        let v = n1 > n2 in
        push(Int(int_of_bool(v)))

    | IS.Let(id) ->
      let v = pop() in
      state.env <- Env.remove id state.env ; (*allow variable shadowing*)
      state.env <- Env.add id v state.env

    | IS.EndLet(id) ->
      state.env <- Env.remove id state.env

    | IS.MkClos(id, e) ->
        let cl = Closure(id, e, state.env) in
        push(cl)
    | IS.If(e1, e2) ->
        let Int v = pop() in
        if v==1 then
          state.code <- e1 @ state.code
        else 
          state.code <- e2 @ state.code

    | IS.While(c, e2) as while_ ->
        let Int v = pop() in
        if v==1 then
          state.code <- e2 @ c @ [while_] @ state.code

    | IS.Apply ->
      let v = pop() in
      let Closure(id, e, env') = pop() in
      let new_cl = Closure("apply_closure", state.code, state.env) in
      push new_cl ;
      state.code <- e @ [IS.Return] ;
      state.env <- Env.add id v env'

    | IS.Return ->
      let v = pop() in
      let c = begin match pop() with Unit -> pop() | x -> x end in
      let Closure(id, e, env') = c in
      push(v);
      state.env <- env';
      state.code <- e 

    | IS.Print ->
        let r, v = match pop() with Ptr(_) | Closure(_,_,_) | Unit -> printf "PrintWarning: Tried to print a non integer, skip instruction...\n" ; 1,0 | Int(x) -> 0,x in
        if r=0 then
          printf "%d\n" v
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
        let ptr = match pop() with
          | Ptr(x) -> x
          | x -> raise (VMError (sprintf "TypeError: %s is not a pointer)" (string_of_value x))) in
	  begin
        try
	      let v = Hashtbl.find state.heap.mem ptr in
          push(v)
	      with Not_found -> raise (VMError (sprintf "PointerError: No such pointer (%d) in heap" ptr)) end 
    | IS.Unit ->
        begin match state.stack with
               | Unit :: s -> ()
               | _ -> push Unit
               end
    | IS.Dup ->
        let v = pop() in
        push(v) ; push(v)
    | IS.Drop -> let _ = pop() in ()
    | IS.Wait -> let thread = Queue.take threads in Queue.add thread threads ; raise Wait_of_thread
    | IS.Spawn ->
        let v = pop() in 
        let Closure(id, e, env') = pop() in
        let new_thread = {code = e; stack=[]; env=(Env.add id v env'); heap=state.heap} in
        Queue.add new_thread threads

let print_stack = List.iter (print_value)

let execute p print_stackval : unit =
  let print_from_stack = function
    | p::_ -> printf "StackPrint: " ; print_value p
    | [] -> () in
  let new_heap () = {address=0; mem=Hashtbl.create heap_size} in
  let threads = Queue.create () in
  let fst_thread = {code=p ; env=Env.empty ; stack=[] ; heap=new_heap()} in
  Queue.add fst_thread threads ;
  while not (Queue.is_empty threads) do 
    let worker = Queue.peek threads in
    begin
    try
      begin
      while true do
        step worker threads done end 
    with End_of_thread(state) ->
      let _ = Queue.take threads in (*remove thread*)
      if print_stackval then print_from_stack state.stack
        | Wait_of_thread -> () 
    end ; done ;;
