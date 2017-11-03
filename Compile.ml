open InstructionSet

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
    (*@ [EndLet(id)]*)

  | Ast.Apply(e1, e2) ->
    (compile_expr e1) @
    (compile_expr e2) @
    [Apply]

  | Ast.Fun(id, e) ->
    let func = compile_expr e in
    [MkClos(id, func)]

  | Ast.Seq(e1, e2) ->
    (compile_expr e1) @
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
  | Ast.Cond(c, e1, e2) ->
      let ce1, ce2 = compile_expr e1, compile_expr e2 in
      (compile_expr c) @
      [If(ce1, ce2)]

let print_prg = 
  List.iter (fun i -> 
    Printf.printf "%s " (string_of_is i))

