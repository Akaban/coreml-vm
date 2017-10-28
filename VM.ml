open Printf
module IS = InstructionSet

module Env = Map.Make(String)
type env = value Env.t
and value =
  | Int of int
  | Unit
  | Closure of string * IS.block * env
  

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
  mutable env   : env
}

exception End_of_thread of thread_state
    

let step state =
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
      let new_cl = Closure("", state.code, state.env) in
      push new_cl ;
      state.code <- e ;
      state.env <- Env.add id v env'

    | IS.Return ->
      let v = pop() in
      let Closure(id, e, env') = pop() in
      push(v);
      state.env <- env';
      state.code <- e
    | _ -> failwith "Not implemented"

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
	

  
