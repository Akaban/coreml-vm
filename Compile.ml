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
    [MkClos(id, func @ [Return])]

  | Ast.Seq(e1, e2) ->
    (compile_expr e1) @
    (compile_expr e2)
  | Ast.SetR(e1, e2) ->
      (compile_expr e1) @ 
      (compile_expr e2) @
      [Store] @
      [Unit]
  | Ast.Spawn(e1, e2) ->
      (compile_expr e1) @
      (compile_expr e2) @
      [Spawn]
  | Ast.Ref(r) ->
      Alloc ::
      Dup ::
      (compile_expr r @ [Store])
  | _ -> assert false 

let print_prg = 
  List.iter (fun i -> 
    Printf.printf "%s, " (string_of_is i))

