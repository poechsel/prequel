open Ast;;

let _ = print_string "hello_world\n"
let parse_line lexbuf = 1;;
let compute ast = 1;;
let display request = "hello world"

let repl () = 
  let lexbuf = Lexing.from_channel stdin
  in let rec aux () = 
       let _ = print_string ">> "; flush stdout
       in let ast = parse_line lexbuf
       in let request = compute ast
       in let _ = display request
       in aux ()
  in aux ();;

let _ = repl()
