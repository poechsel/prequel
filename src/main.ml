open Ast
open Lexer
open Parser
open Errors



let parse_line lexbuf =
  try
    Parser.main Lexer.token lexbuf
  with exn ->
    begin
      let tok = Lexing.lexeme lexbuf in
      raise (send_parsing_error (Lexing.lexeme_start_p lexbuf) tok)
    end


let _ = print_string "hello_world\n"
let compute ast = 1;;
let display request = "hello world"

let repl () = 
  let lexbuf = Lexing.from_channel stdin
  in let rec aux () = 
       let _ = print_string ">> "; flush stdout
       in let _ = try
          let ast = parse_line lexbuf
          in let request = compute 1
          in let _ = display request
          in ()
        with ParsingError x ->
          let _ = Lexing.flush_input lexbuf
          in let _ = Parsing.clear_parser ()
          in let _ = print_endline x 
          in ()
       in aux ()
  in aux ();;

let _ = repl()
