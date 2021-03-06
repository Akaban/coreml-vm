# CoreML Virtual Machine

This program is a OCaml implemented Virtual Machine capable of running simple CoreML code, it reads a .cml file as its input then feed it to the built-in compiler in order to run it with the Virtual Machine

## Getting Started

  * A recent version of OCaml is required
  * `ocaml-findlib`, `ocamldep` and `ocamlbuild` are required
  * `ocamllex` and `ocaml-menhir` are required
  * If all requirements of the above are meet, compile the project with `make`
  * The result binary will be named `VM`

## Example .cml

The following CoreML code will compute the n-th Fibonacci number, the argument n is to be set
in `fibo_arg`

    let fibo_arg=10 in
    let fibo_arg=ref fibo_arg in

    let fibo1=ref 1 in
    let fibo2=ref 1 in

    while !fibo_arg > 2 do
      let tmp = !fibo2 in
      fibo_arg <- !fibo_arg - 1 ;
      fibo2 <- !fibo1 + !fibo2 ;
      fibo1 <- tmp done ;
      print !fibo2

     

The compiled VM program is,

    10 Let(fibo_arg) Alloc Dup fibo_arg Store Let(fibo_arg) Alloc Dup 1 Store Let(fibo1) Alloc Dup 1 Store Let(fibo2) 2 fibo_arg Load > 
    While {2, fibo_arg, Load, >} do 
      {fibo2, Load, Let(tmp), fibo_arg, 1, fibo_arg, Load, -, Store, (), fibo2, fibo2, Load, fibo1, Load, +, Store, (), fibo1, tmp, Store, ()} 
    fibo2 Load Print

This will return the value 55.

Others example of code can be found in the folder `tests`. We can run some of these tests with the script `runtest.sh`

## What is implemented?

  * Integers, Pointers and Functions
  * All basic binary operations (add, sub, mul, integer division, <, >, >=, <=, =)
  * Booleans as integers (C-like), 1 is true and everything else is considered false. And (&&), Or (||) are also implemented
  * References (pointers) with the keyword `ref` ie. `let a = ref 10`.
    We can then refer to a reference with a bang like this `!a` or set the value of a reference with the following syntax
    `a <- !a + 1`
  * Conditionals (if) and loops (while, for).
  * Increasing for loop syntax is `for a=1 upto 10 do expr done`. We can use a decreasing loop with `downto` instead of `upto`
  * Concurrent programming (the implementation is basic though), with the keyword `spawn fun arg`. We can wait all threads to finish with the keyword `wait`
  * Anonymous (lambdas) functions: `let a = (fun b -> b + 1)`. If this function has a name we can refer to this function inside of itself recursively (no need for `let rec` just `let`)
  * Printing integers (with `print`)

## Command line arguments

It may be interesting to separate the normal stdout printing from the stderr printing (warnings and debug are printed in stderr) with redirections,

      usage: ./VM file.cml
    --print-program Print compiled program
    --no-exec Do not execute the compiled program on the VM
    --print-stack After a thread ends, print its stack's last value
    --debug Verbose VM mode, print each reduction step with its stack state
    -help  Display this list of options
    --help  Display this list of options






