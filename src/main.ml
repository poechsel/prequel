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
  let query = "select test.Title2 from \"test.csv\" test, \"test2.csv\" test2 where test.Title1 + 1 = 2;" in
  let query = "select foo.Title2 from (SELECT * from \"test.csv\" test, \"test2.csv\" test2 where test.Title1 + 1 = 2) AS foo;" in
  let query = "SELECT e.nom, d.nom FROM \"employes.csv\" e, \"departements.csv\" d WHERE e.dpt = d.idd;" in
  let query =
    "SELECT e.dpt, e.nom FROM \"employes.csv\" e WHERE e.dpt IN ( SELECT s.dpt FROM \"employes.csv\" s, \"departements.csv\" ds WHERE ds.directeur = s.ide AND e.dpt = ds.idd);" in 
  (*let query =
    "SELECT s.dpt FROM \"employes.csv\" s, \"departements.csv\" ds WHERE ds.directeur = s.ide;" in *)
  let lexbuf = Lexing.from_string query in
  let ast = parse_line lexbuf in
  let ast_disj = Transformers.disjunction ast in
  let alg = Naivecompiler.naive_compiler ast_disj in
  let graphviz_src = Debug.graphviz_of_algebra "test.dot" alg in
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
