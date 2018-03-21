open Ast
open Lexer
open Parser
open Errors
open MetaQuery


(** parse_input : in_channel -> Command.t
    Attemps to parse a command from the standard input. *)
let parse_input channel =
  let buffer = Lexing.from_channel channel in

  try
    let command = Parser.main Lexer.token buffer in
    Lexing.flush_input buffer;
    Parsing.clear_parser ();
    command
  with _ ->
    let tok = Lexing.lexeme buffer in
    raise (send_parsing_error (Lexing.lexeme_start_p buffer) tok) (* TODO *)


(** run_query : Ast.t -> unit
    Attempts to execute a query using the interpreter. *)
let run_query ?debug:(debug=false) ?output:(output=stdout) query =
  let algebra =
    query
    |> AstChecker.check_coherence
    |> AstChecker.rename_tables
    |> AstTransformers.disjunction
    |> Compiler.compile in

  (* In debug mode, display a graph of the algebra term. *)
  if debug then begin
    let (name, chan) = Filename.open_temp_file "minisql" "graph" in
    Debug.graphviz_of_algebra chan algebra;
    close_out chan;

    let name' = Filename.temp_file "minisql" "pdf" in
    Printf.sprintf
      "dot -Tpdf %s -o %s && xdg-open %s"
      name name' name'
    |> Sys.command
    |> ignore
  end;

  let feed = MetaQuery.feed_from_query algebra in
  feed#save output;
  flush output