open Ast
open Lexer
open Parser
open Errors



let parse_line lexbuf =
  (* parse a line from a corresponding buffer of tokens
     @Param lexbuf: a buffer of tokens
     *)
  try
    Parser.main Lexer.token lexbuf
  with exn ->
    begin
      let tok = Lexing.lexeme lexbuf in
      raise (send_parsing_error (Lexing.lexeme_start_p lexbuf) tok)
    end



let action ast = 
  (* the main body defining the different operations made to execute a sql query
     @Param ast: a parsed sql query
    *)
  let _ = Transformers.identity ast in
  print_string "hello world"


let repl () = 
  (* Repl interfact *)
  let lexbuf = Lexing.from_channel stdin in
  let rec aux () = 
       let _ = print_string ">> "; flush stdout in 
       let _ = try
        let ast = parse_line lexbuf in
          action ast
        with ParsingError x ->
          let _ = Lexing.flush_input lexbuf in 
          let _ = Parsing.clear_parser () in 
          let _ = print_endline x in 
          ()
       in 
       aux ()
  in 
  aux ();;

let _ = repl()
