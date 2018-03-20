open Common
open OUnit2
open Csv


(** The different modes for checking whether the
    output of MiniSQL matches the expected output. *)
type mode =
  | Normal  (* Each expected line must appear at least one. *)
  | Ordered (* Same as Normal, but in the right order. *)
  | Exact   (* Output must be identical to the expected output. *)

let mode_of_line s =
  match String.lowercase s with
    | "[normal]"  -> Normal
    | "[ordered]" -> Ordered
    | "[exact]"   -> Exact
    | _ -> failwith "Unknown test mode."


(** list_files : unit -> (string, in_channel) list
    Returns a list of all the test files, contained
    in the subfolders of the `tests/` directory. *)
let list_files () =
  let is_test_file s =
    String.sub s ((String.length s) - 4) 4 = ".sql" in

  let rec aux acc = function
    | x :: xs when Sys.is_directory x ->
        Sys.readdir x
        |> Array.to_list
        |> List.map (Filename.concat x)
        |> List.append xs
        |> aux acc
    | x :: xs when is_test_file x ->
        aux ((x, open_in x) :: acc) xs
    | x :: xs -> aux acc xs
    | [] -> acc in

  aux [] ["tests"]


(** make_case : string -> OUnit2.test_fun
    Builds a test case function for a given test
    file. This function will check whether the
    output of MiniSQL on the query from the file
    is the same as the expected output. *)
let make_case file =
  let rec parse_line line prev =
    if String.length line >= 1 &&
       String.sub line 0 1 = "[" then
      (prev, mode_of_line line, Csv.of_channel file |> Csv.input_all)
    else
      parse_line (input_line file) (prev ^ " " ^ line) in

  fun _ ->
    let (query, mode, csv) =
      parse_line (input_line file) "" in

    let params = {
      repl = ref false;
      out = ref "";
      request = ref "";
      graphviz = ref ""} in

    let buffer = Lexing.from_string query in
    let ast = Common.parse_line ~with_endline:false buffer in
    Common.action params ast;

    let csv' = Csv.of_channel file |> Csv.input_all in
    assert_equal 0 @@ Csv.compare csv csv'


let () =
  let cases = list_files ()
    |> List.map (fun (x, file) -> x >:: make_case file) in
  let suite = "suite" >::: cases in
  run_test_tt_main suite
