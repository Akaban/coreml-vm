open InstructionSet

let rec compile_expr = function
  | Ast.Int(n) ->
    [Int(n)]

  | Ast.Ident(id) ->
    [Lookup(id)]

  | Ast.Binop(Ast.Add, e1, e2) ->
    (* D'abord un opérande, puis
       l'autre, puis l'opérateur,
       comme en notation
       polonaise inversée. *)
    (compile_expr e2) @
    (compile_expr e1) @
    [Add]
  | Ast.Binop(Ast.Sub, e1, e2) ->
      (compile_expr e2) @
      (compile_expr e1) @
      [Sub]
  | Ast.Binop(Ast.Mult, e1, e2) ->
    (compile_expr e2) @
    (compile_expr e1) @
    [Mult]

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
  | Ast.Ref(r) ->
      Alloc ::
      Dup ::
      (compile_expr r @ [Store])
  (*| Ast.While(id, e) ->
      let b = compile_expr e in
      [While(id,e)]*)
  | Ast.Cond(c, e1, e2) ->
      let ce1, ce2 = compile_expr e1, compile_expr e2 in
      (compile_expr c) @
      [If(ce1, ce2)]
  | _ -> failwith "Not implemented"

let print_prg = 
  List.iter (fun i -> 
    Printf.printf "%s, " (string_of_is i))

