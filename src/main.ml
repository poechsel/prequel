open Print
open Utils
open Common
open Errors
open Command


(** REPL state variables. *)
let debug = ref false


(** print_header : unit -> unit
    Prints the welcome message to the standard output. *)
let print_header () =

  "           _       _           _     \n" ^
  "          (_)     (_)         | |    \n" ^
  " _ __ ___  _ _ __  _ ___  __ _| |    \n" ^
  "| '_ ` _ \\| | '_ \\| / __|/ _` | |  \n" ^
  "| | | | | | | | | | \\__ \\ (_| | |  \n" ^
  "|_| |_| |_|_|_| |_|_|___/\\__, |_|   \n" ^
  "                            | |      \n" ^
  "                            |_|      \n"
  |> blue
  |> print_endline;

  "MiniSQL version 1.1.                 \n" ^
  "Enter .help for usage tips.          \n" ^
  "Enter .debug to turn on debug output.\n"
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
let run_command = function
  | Command "help"  ->
      let message =
        "          .help - Displays this message.\n" ^
        "           .pwd - Prints the current working directory.\n" ^
        "     .cd {path} - Changes the current working directory.\n" ^
        ".debug {on/off} - Enables or disables debug output.\n" in

      print_endline <| faint message

  | Command "debug" ->
      debug := true;
      print_endline <| faint "Debug output enabled.";
      print_newline ()

  | Command _ -> print_error "Unknown command."
  | Query q ->
      run_query ~debug:!debug q;
      print_newline ()


(** start_repl : unit -> unit
    Runs MiniSQL as a REPL loop. *)
let start_repl () =
  print_header ();

  while true do
    print_string <| bold "> ";
    flush stdout;

    try
      parse_input stdin
      |> run_command
    with
      | ParsingError (s)        -> print_error <| "Syntax error: " ^ s
      | SemanticError (s)       -> print_error <| "Query semantic error: " ^ s
      | InterpretationError (s) -> print_error <| "Interpretation error: " ^ s
  done


(** start_single : in_channel -> unit
    Runs MiniSQL on a single file input. *)
let start_single chan =
  match parse_input chan with
  | Query q -> run_query q
  | _       -> failwith "Not a valid SQL query."


let () =
  let path = ref "" in
  let usage =
    "MiniSQL version 1.1.\n" ^
    "Usage: ./minisql [path]\n" ^
    "When path is not specified, runs in REPL mode." in

  Arg.parse [] ((:=) path) usage;

  if !path = "" then
    start_repl ()
  else
    start_single <| open_in !path