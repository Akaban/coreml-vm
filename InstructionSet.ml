type instruction =
  | Int of int
  | Lookup of string
  | Add
  | Mult
  | Sub
  | MkClos of string * block
  | Let of string
  | EndLet of string
  | Return
  | Apply
  | Alloc
  | Unit
  | Load
  | Store

and block = instruction list


let string_of_is = function
	| Int(n) -> string_of_int n
	| Lookup(id) -> id
	| Add -> "+"
	| Sub -> "-"
	| Mult -> "*"
	| Let(id) -> Printf.sprintf "Let %s" id
	| EndLet(id) -> Printf.sprintf "EndLet %s" id
	| MkClos(id, e) -> "MkClos"
	| Apply -> "Apply"
	| Return -> "Return"
  | Alloc -> "Alloc" 
  | Load -> "Load"
  | Store -> "Store"

let print_prg = 
  List.iter (fun i -> 
    Printf.printf "%s, " (string_of_is i))

