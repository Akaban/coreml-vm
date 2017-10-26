type instruction =
  | Int of int
  | Lookup of string
  | Add

and block = instruction list
