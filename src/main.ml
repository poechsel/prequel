open Ast
open Lexer
open Parser
open Errors

open MetaQuery


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

(*example:
  select test.Title1 from test;
  select * from test;
  *)

let _ = 
  let query = "select * from test where test.Title1 == \"one\";" in
  let lexbuf = Lexing.from_string query in
  let ast = parse_line lexbuf in
  let alg = Naivecompiler.naive_compiler ast in
  let feed = MetaQuery.feed_from_query alg in
  let rec aux () = 
       match feed#next with
       | None -> ()
       | Some i -> List.iter (fun x ->  Printf.printf "%s, " x) i; Printf.printf "\n"; aux ()
  in aux ()
  in ()


(*
let _ = 
  let feed = MetaQuery.feed_from_query (AlgebraTypes.AlgProjection ((AlgebraTypes.AlgUnion 
                                                (AlgebraTypes.AlgInput "test", 
                                                 AlgebraTypes.AlgInput "test")
                                                        ), ["Title1"]))
  in let rec aux () = 
       match feed#next with
       | None -> ()
       | Some i -> List.iter (fun x ->  Printf.printf "%s, " x) i; Printf.printf "\n"; aux ()
  in aux ()
  in ()
*)

(*
let _ = 
  let ic = open_in "test.csv" in
  let csv = Csv.of_channel ?has_header:(Some true) ic in
  let _ = List.iter (fun x -> Printf.printf "%s, " x) (Csv.Rows.header csv) in
  let _ = Printf.printf "\n" in
  let rec aux () = 
    try
      let line = Csv.Row.to_list (Csv.Rows.next csv) in
      let _ = List.iter (fun x -> Printf.printf "%s, " x) line in
      let _ = Printf.printf "\n" in
      aux ()
    with Csv.Failure _ ->
      ()
  in aux ()
*)

(* let _ = repl() *)
