type instruction =
  | Int of int | Lookup of string | Add | Mult | Div | Sub | And | Or
  | MkClos of string * block | Let of string | EndLet of string | Return | Apply of string option
  | Alloc | Unit | Drop | Spawn | Wait | Load | Store | Dup
  | While of block * block | If of block * block | Eq | Geq | Leq | Gt | Lt
  | Print


and block = instruction list

let rec string_of_is = 
  let rec string_of_block = function
      | [x] -> string_of_is x
      | x::xs -> (string_of_is x) ^ ", " ^ (string_of_block xs)
      | [] -> "" in 
  function
    | Int(n) -> string_of_int n
    | Lookup(id) -> id
    | Add -> "+"
    | Sub -> "-"
    | Div -> "/"
    | Mult -> "*"
    | Eq -> "=="
    | And -> "&&"
    | Or -> "||"
    | Geq -> ">="
    | Leq -> "<="
    | Lt  -> "<"
    | Gt -> ">"
    | Print -> "Print"
    | Let(id) -> Printf.sprintf "Let(%s)" id
    | EndLet(id) -> Printf.sprintf "EndLet(%s)" id
    | MkClos(id, b) ->
        Printf.sprintf "MkClos %s { [%s]}" id (string_of_block b)
    | While(cond, b) -> 
        Printf.sprintf "While {%s} do {%s}" (string_of_block cond) (string_of_block b)
    | If(e1, e2) ->
        Printf.sprintf "If true then {%s} else {%s}" (string_of_block e1) (string_of_block e2)
    | Apply(_) -> "Apply"
    | Return -> "Return"
    | Alloc -> "Alloc" 
    | Load -> "Load"
    | Store -> "Store"
    | Dup -> "Dup"
    | Spawn -> "Spawn"
    | Drop -> "Drop"
    | Wait -> "Wait"
    | Unit -> "()"


