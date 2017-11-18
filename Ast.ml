type expr =
  | Int   of int
  | Ident of ident
  | Binop of binop * expr * expr
  | Letin of ident * expr * expr
  | Fun   of ident * expr
  | Apply of expr * expr
  | Cond  of expr * expr * expr
  | Loop  of expr * expr
  | ForU   of ident * expr * expr * expr
  | ForD   of ident * expr * expr * expr
  | For   of ident * expr * expr * expr * bool
  | Seq   of expr * expr
  | Ref   of expr
  | GetR  of expr
  | SetR  of expr * expr
  | Spawn of expr * expr
  | Print of expr
  | Wait
and ident = string
and binop = Add | Sub | Mult | Div | Eq | Geq | Leq | Gt | Lt | And | Or
