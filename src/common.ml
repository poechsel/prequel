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
    raise @@ make_syntax_error
      (Lexing.lexeme_start_p buffer)
      (Lexing.lexeme buffer)


(** run_query : Ast.t -> unit
    Attempts to execute a query using the interpreter. *)
let run_query ?debug:(debug=false) ?pretty:(pretty=false) ?output:(output=stdout) ?graph:(graph=None) query =
  let algebra =
    query
    |> AstChecker.check_coherence
    |> AstChecker.rename_tables
    |> AstTransformers.disjunction
    |> Compiler.compile 
    |> OptimisationPass.push_down_select
    |> OptimisationPass.create_joins
    |> OptimisationPass.select_compressor
    |> OptimisationPass.optimize_projections
  in

  (* In debug mode, display a graph of the algebra term. *)
  if debug || graph <> None then begin
    let (name, chan) = Filename.open_temp_file "minisql" "graph" in
    Debug.graphviz_of_algebra chan algebra;
    close_out chan;

    let name' = match graph with
      | Some path -> path
      | None      -> Filename.temp_file "minisql" "pdf" in

    Printf.sprintf
      "dot -Tpdf %s -o %s"
      name name'
    |> Sys.command
    |> ignore;

    if debug then begin
      print_endline "Terme relationnel :";
      print_endline @@ AlgebraTypes.show_algebra algebra;

      Printf.sprintf
        "xdg-open %s"
        name'
      |> Sys.command
      |> ignore
    end 
  end;

  let feed = MetaQuery.feed_from_query algebra in
  if pretty then
    feed#print
  else begin
    feed#save output;
    flush output
  end
