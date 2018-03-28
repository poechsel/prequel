open Print
open Common
open Errors
open Command


(** REPL state variables. *)
let wd = Sys.getcwd ()
let debug = ref false
let pretty = ref false


(** print_header : unit -> unit
    Prints the welcome message to the standard output. *)
let print_header () =
  "                                                    888\n" ^
  "                                                    888\n" ^
  "88888b.  888d888 .d88b.   .d88888 888  888  .d88b.  888\n" ^
  "888  88b 888P   d8P  Y8b d88  888 888  888 d8P  Y8b 888\n" ^
  "888  888 888    88888888 888  888 888  888 88888888 888\n" ^
  "888 d88P 888    Y8b.     Y88b 888 Y88b 888 Y8b.     888\n" ^
  "88888P   888      Y8888    Y88888   Y88888   Y8888  888\n" ^
  "888                           888                      \n" ^
  "888                           888                      \n"
  |> blue
  |> print_endline;

  "Prequel version 1.1.                 \n" ^
  "Enter .help; for usage tips.         \n"
  |> faint
  |> print_endline


(** print_error : str -> unit
    Prints an error message to the standard output. *)
let print_error e =
  "[ERROR] " ^ e
  |> err
  |> print_endline


(** run_command : Command.t -> unit
    Attemps to run a top-level command. *)
let run_command ?opti:(opti=make_optimizations ()) command =
  match command with
  | Command ("help", None) ->
      print_string @@
        (bold "           .help;") ^ (faint " Displays this message.\n") ^
        (bold "            .pwd;") ^ (faint " Prints the current working directory.\n") ^
        (bold "      .cd {path};") ^ (faint " Changes the current working directory.\n") ^
        (bold " .debug {on/off};") ^ (faint " Enables or disables debug output.\n") ^
        (bold ".pretty {on/off};") ^ (faint " Enables or disables pretty printing.\n")

  | Command ("debug", Some "on") ->
      debug := true;
      print_endline @@ faint "Debug output enabled."

  | Command ("debug", Some "off") ->
      debug := false;
      print_endline @@ faint "Debug output disabled."

  | Command ("pretty", Some "on") ->
      pretty := true;
      print_endline @@ faint "Pretty printing enabled."

  | Command ("pretty", Some "off") ->
      pretty := false;
      print_endline @@ faint "Pretty printing disabled."

  | Command ("cd", Some dir) ->
      begin try
        Sys.chdir dir;
        print_endline @@ faint "Changed working directory."
      with Sys_error _ ->
        print_error "The directory doesn't exist."
      end

  | Command ("pwd", None) ->
      print_endline @@ Sys.getcwd ()

  | Command (_, _)  -> print_error "Unknown command."
  | Query q       -> run_query ~opti:opti ~debug:!debug ~pretty:!pretty q


(** start_repl : unit -> unit
    Runs MiniSQL as a REPL loop. *)
let start_repl ?opti:(opti=make_optimizations ()) () =
  print_header ();

  while true do
    if Sys.getcwd () <> wd then begin
      let root = Fpath.v @@ wd in
      let curr = Fpath.v @@ Sys.getcwd () in
      match Fpath.relativize ~root curr with
        | Some rel ->
            Printf.printf "(%s)" (Fpath.to_string rel)
        | _ -> ()
    end;

    print_string @@ bold "> ";
    flush stdout;

    begin try
      parse_input stdin
      |> run_command ~opti:opti
    with
      | SyntaxError (s)         -> print_error @@ "Syntax error: " ^ s
      | SemanticError (s)       -> print_error @@ "Query semantic error: " ^ s
      | InterpretationError (s) -> print_error @@ "Interpretation error: " ^ s
    end;

    print_newline ()
  done


(** start_single : str -> str -> str -> unit
    Runs MiniSQL on a single file input. *)
let start_single ?opti:(opti=make_optimizations ()) file output graph =
  let chan = open_in file in
  let output =
    if output = "" then
      stdout
    else
      open_out output in
  let graph =
    if graph = "" then
      None
    else
      Some graph in

  match parse_input chan with
  | Query q -> run_query ~opti:opti ~output ~graph q
  | _       -> failwith "Not a valid SQL query."


let () =
  let path = ref "" in
  let output = ref "" in
  let graph = ref "" in
  let opti = make_optimizations () in

  let usage =
    "Prequel version 1.1.\n" ^
    "Usage: ./prequel [path]\n" ^
    "When path is not specified, runs in REPL mode." in

  let speclist = [
    "--output", Arg.Set_string output, "A file in which to write the output.";
    "--graph", Arg.Set_string graph, "A file in which to save a graph of the term.";
    "--use-caching", Arg.Set opti.use_caching, "Enable caching optimization.";
    "--no-select-push-down", Arg.Set opti.no_select_pd, "Disable push down of selections";
    "--no-projection-opti", Arg.Set opti.no_projections, "Disable optimisations of projections";
    "--no-joins", Arg.Set opti.no_joins, "Disable joins creations";
  ] in

  Arg.parse speclist ((:=) path) usage;

  if !path = "" then
    start_repl ~opti:opti ()
  else
    start_single
      ~opti: opti
      !path
      !output
      !graph
