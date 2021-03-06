open InstructionSet

let _ = Random.self_init()

let rand_char() =
  let rand_n = 65 + (Random.int 26) in
  Char.chr rand_n

let repeat =
  let rec doRepeat f = function
    | x when x <= 0 -> ()
    | x -> f() ; doRepeat f (x-1) in
  doRepeat

let rand_string n =
  let res = ref "" in
  if n <= 0 then "" else begin
  repeat (fun () -> res := !res ^ (Char.escaped (rand_char()))) n ; !res end




(* compile_expr :: Ast.t -> IS list
 * Compile the AST tree encoded program towards the VM instructions
 * Return the compiled program as an instruction list*)

let rec compile_expr = function
  | Ast.Int(n) ->
    [Int(n)]

  | Ast.Ident(id) ->
    [Lookup(id)]

  | Ast.Binop(op, e1, e2) ->
      let compile_op = begin match op with
        | Ast.Add -> [Add]
        | Ast.Sub -> [Sub]
        | Ast.Mult -> [Mult]
        | Ast.Div -> [Div]
        | Ast.And -> [And]
        | Ast.Or -> [Or]
        | Ast.Eq -> [Eq]
        | Ast.Geq -> [Geq]
        | Ast.Leq -> [Leq]
        | Ast.Gt -> [Gt]
        | Ast.Lt -> [Lt] end in
      (compile_expr e2) @
      (compile_expr e1) @
      compile_op

  | Ast.Letin(id, e1, e2) ->
    (compile_expr e1) @
    [Let(id)] @
    (compile_expr e2)

  | Ast.Apply(e1, e2) ->
    let fun_expr = compile_expr e1 in
    let fun_ref = match fun_expr with [Lookup(id)] -> Some id | _ -> None in
    (compile_expr e1) @
    (compile_expr e2) @
    [Apply(fun_ref)]

  | Ast.Fun(id, e) ->
    let func = compile_expr e in
    [MkClos(id, func)]

  | Ast.Seq(e1, e2) ->
    (compile_expr e1) @
    [Drop] @
    (compile_expr e2)
  | Ast.SetR(e1, e2) ->
      (compile_expr e1) @ 
      (compile_expr e2) @
      [Store] @
      [Unit]
  | Ast.GetR(e) ->
      (compile_expr e) @
      [Load]
  | Ast.Spawn(e1, e2) ->
      (compile_expr e1) @
      (compile_expr e2) @
      [Spawn]
  | Ast.Wait -> [Wait]
  | Ast.Ref(r) ->
      Alloc ::
      Dup ::
      (compile_expr r @ [Store])
  | Ast.Loop(c, e) ->
      let b = compile_expr e in
      let c' = compile_expr c in
      c' @ 
      [While(c',b)]

  | Ast.For(id, begfor, endfor, e1, direction) -> (* direction = True -> UPTO *)
     let comp_op, loop_op = if direction then Leq, Add else Geq, Sub in (* direction = False -> DOWNTO *)
     let ptr_id = Printf.sprintf "%s_%s" (id) (rand_string 8) in
     let endfor', begfor' = compile_expr endfor, compile_expr begfor in
     let condWhile = endfor' @ [Lookup(ptr_id); Load; comp_op] in
     let e1' = [Lookup(ptr_id);Load;Let(id)] @
       (compile_expr e1) @ [Lookup(ptr_id); Int(1); Lookup(ptr_id); Load; loop_op; Store; Unit; EndLet(id)] (*variable incrementation code*) in
     (*syntaxic sugar, we compile this using a while instruction*)
     Alloc ::
     Dup ::
     (begfor' @ 
     [ Store ;
     Let(ptr_id) ] @
     condWhile @
     [While(condWhile,e1')])

    | Ast.Print(e) ->
      (compile_expr e) @
      [Print]

  | Ast.Cond(c, e1, e2) ->
      let ce1, ce2 = compile_expr e1, compile_expr e2 in
      (compile_expr c) @
      [If(ce1, ce2)]


let print_prg =
  let rec printfun = function
    | [x] -> Printf.printf "%s" (string_of_is x)
    | x::xs -> Printf.printf "%s, " (string_of_is x) ; printfun xs
    | [] -> () in
  printfun

