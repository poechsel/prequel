open Ast
open Lexer
open Parser
open Errors
open MetaQuery


type optimizations = {
  use_caching : bool ref;
  no_select_pd : bool ref;
  no_projections : bool ref;
  no_joins : bool ref;
  big_data : bool ref; }


(** make_optimizations : unit -> optimizations
    Creates a default set of optimisation options. *)
let make_optimizations () = {
    use_caching = ref false;
    no_select_pd = ref false;
    no_projections = ref false;
    no_joins = ref false;
    big_data = ref false; }


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


let execute_if bo fct =
  (* if bo = true then return fun x -> fct x,
     otherwise return fun x -> x*)
  if bo then
    fun x -> fct x
  else 
    fun x -> x

(** run_query : Ast.t -> unit
    Attempts to execute a query using the interpreter. *)
let run_query 
    ?debug:(debug=false) 
    ?pretty:(pretty=false) 
    ?output:(output=stdout) 
    ?graph:(graph=None) 
    ?opti:(opti=make_optimizations ()) 
    ?basedir:(basedir=".")
    query =
  (* Set the correct working directory. *)
  let cwd' = Sys.getcwd () in
  Sys.chdir basedir;

  let algebra =
    query
    |> AstChecker.check_coherence
    |> AstChecker.rename_tables
    |> AstChecker.extract_aggregates
    |> AstTransformers.disjunction
    |> Compiler.compile ~generate_joins:(not !(opti.no_joins))
    |> execute_if (not !(opti.no_select_pd)) OptimisationPass.push_down_select
    |> execute_if (not !(opti.no_joins)) OptimisationPass.create_joins
    |> OptimisationPass.select_compressor
    |> execute_if (not !(opti.no_projections)) OptimisationPass.optimize_projections
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

  let feed = MetaQuery.feed_from_query ~use_caching:!(opti.use_caching) ~big_data:!(opti.big_data) algebra in
  if pretty then
    feed#print
  else begin
    feed#save output;
    flush output
  end;

  (* Restore the previous working directory. *)
  Sys.chdir cwd'