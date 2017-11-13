let printp = ref false
let noexec = ref false
let printstack = ref false
let debug = ref false

let usage = "usage: ./VM file.cml"
let spec  = 
  [ "--print-program", Arg.Set printp, "Print compiled program" ;
    "--no-exec", Arg.Set noexec, "Do not execute the compiled program on the VM";
    "--print-stack", Arg.Set printstack, "After a thread ends, print its stack's last value";
    "--debug", Arg.Set debug, "Verbose VM mode, print each reduction step with its stack state"]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".cml") then
      raise (Arg.Bad "no .cml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> exit 1

let () =
  let _ = Arg.align spec in
  let c  = open_in file in
  let lb = Lexing.from_channel c in
  let e  = Parser.main Lexer.token lb in
  close_in c;
  let p  = Compile.compile_expr e in begin
  if !printp then
    (print_string "Compiled prog = " ; Compile.print_prg p ; print_newline()) end ;
  (if (not !noexec) then
    begin
    try
    VM.execute p !printstack !debug
    with
      | Match_failure (_,_,_) -> raise (VM.VMError "SemanticError: The Virtual Machine encoutered a MatchError because of a semantic error") end) ;
  exit 0
