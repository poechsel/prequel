open Ast
open Lexer
open Parser
open Errors

open MetaQuery

type params = {repl: bool ref; out: string ref; request: string ref; graphviz: string ref}

let parse_line ?(with_endline=true) lexbuf =
  (* parse a line from a corresponding buffer of tokens
     @Param lexbuf: a buffer of tokens
     *)
  try
    if with_endline = true then
      Parser.main_with_endline Lexer.token lexbuf
    else 
      Parser.main_without_endline Lexer.token lexbuf
  with exn ->
    begin
      let tok = Lexing.lexeme lexbuf in
      raise (send_parsing_error (Lexing.lexeme_start_p lexbuf) tok)
    end

let clean_ast ast = 
  let ast = AstChecker.check_coherence ast in
  let ast = AstChecker.rename_tables ast in
  let ast_disj = AstTransformers.disjunction ast in
  ast_disj

let compile_and_optimize ast =
  let alg = Compiler.compile ast in
  alg


let action params ast = 
  (* the main body defining the different operations made to execute a sql query
     @Param ast: a parsed sql query
    *)
  let ast = clean_ast ast in
  let alg = compile_and_optimize ast in
  let _ = if !(params.graphviz) <> "" then 
      let channel = open_out !(params.graphviz) in 
      let _ = Debug.graphviz_of_algebra channel alg in 
      close_out channel 
  in
  let feed = MetaQuery.feed_from_query alg in
  let feed = new ExternalSort.sort (feed) ([AlgAtom(Attribute("1", "id1")); AlgAtom(Attribute("1","id2"))]) in
  let out_channel = if !(params.out) = "" then stdout else open_out !(params.out) in
  let _ = feed#save out_channel in
  ()


let repl params = 
  (* Repl interfact *)
  let lexbuf = Lexing.from_channel stdin in
  let rec aux () = 
    let _ = print_string ">> "; flush stdout in 
    let _ = try
        let ast = parse_line lexbuf in
        action params ast
      with 
      | ParsingError x ->
        let _ = Lexing.flush_input lexbuf in 
        let _ = Parsing.clear_parser () in 
        let _ = print_endline x in 
        ()
      | BadQuery x ->
        let _ = print_endline x in
        ()
      | InterpretationError x ->
        let _ = print_endline x in
        ()
    in 
    aux ()
  in 
  aux ();;


let _ = 
  let params = {repl = ref false; out = ref ""; request = ref ""; graphviz = ref ""} in
  let speclist = [
    "-repl", Arg.Set params.repl, "use the repl mode"
    ; "-o", Arg.Set_string params.out, "file in which to write the output"
    ; "-graphviz", Arg.Set_string params.graphviz, "generate a graphviz plot of the relationnal algebra tree"
  ] in
  let () = Arg.parse speclist ((:=) params.request) "MinSQl" in


  if !(params.repl) || !(params.request) = "" then 
      repl params
    else 
      let input_channel = 
        try
          open_in !(params.request)
        with e ->
          Printf.printf "File \"%s\" doesn't exists" !(params.request);
          exit 1
      in
  let lexbuf = Lexing.from_channel input_channel in
  let ast = parse_line ~with_endline:false lexbuf in
  action params ast

let _ = TempManager.remove_all_temp ()
