type instruction =
  | Int of int | Lookup of string | Add | Mult| Sub
  | MkClos of string * block | Let of string | EndLet of string | Return | Apply
  | Alloc | Unit | Drop | Spawn | Load | Store | Dup
  | While of block * block | If of block * block


and block = instruction list

let rec string_of_is = 
  let string_of_block b =
  List.fold_right (fun x acc -> (string_of_is x) ^ ", " ^ acc) b "" in
  function
  | Int(n) -> string_of_int n
  | Lookup(id) -> id
  | Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Let(id) -> Printf.sprintf "Let %s" id
  | EndLet(id) -> Printf.sprintf "EndLet %s" id
  | MkClos(id, b) ->
      (Printf.sprintf "MkClos %s { [" id) ^ string_of_block b ^ "]}"
  | While(cond, b) -> ""
  | If(e1, e2) ->
      (Printf.sprintf "If true then {") ^ string_of_block e1 ^ " } else { " ^ string_of_block e2 ^ "}"
  | Apply -> "Apply"
  | Return -> "Return"
  | Alloc -> "Alloc" 
  | Load -> "Load"
  | Store -> "Store"
  | Dup -> "Dup"
  | Spawn -> "Spawn"
  | Drop -> "Drop"
  | Unit -> "()"


