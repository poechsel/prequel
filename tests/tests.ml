open Common
open OUnit2
open Csv


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


(** The different modes for checking whether the
    output of MiniSQL matches the expected output. *)
type mode =
  | Normal  (* Each expected line must appear at least one. *)
  | Ordered (* Same as Normal, but in the right order. *)
  | Exact   (* Output must be identical to the expected output. *)
  | Todo    (* The test should be ignored. *)


let mode_of_line s =
  match String.lowercase s with
    | "[normal]"  -> Normal
    | "[ordered]" -> Ordered
    | "[exact]"   -> Exact
    | "[todo]"    -> Todo
    | _ -> failwith "Unknown test mode."


(** Compares two CSV files with a given test mode. *)
let compare_csvs mode a b =
  let printer = String.concat ", " in

  (* Compare the two headers *)
  let ha = Csv.Rows.header a in
  let hb = Csv.Rows.header b in
  if ha <> hb then
    Printf.sprintf
      "Headers didn't match. Expected `%s` but got `%s`."
      (printer ha)
      (printer hb)
    |> assert_failure;

  (* Compare the rows *)
  let ra = Csv.Rows.input_all a |> Array.of_list in
  let rb = Csv.Rows.input_all b |> Array.of_list in
  match mode with
    | Todo   -> todo "The test is not finished yet."
    | Normal ->
        (* In normal mode, we use a hashtable to count the
           number of occurences of each row in the result. *)
        let occs = Hashtbl.create @@ Array.length ra in
        ra |> Array.iter (fun row ->
          let row = Csv.Row.to_list row in
          Hashtbl.replace occs row 0);

        rb |> Array.iter (fun row ->
          let row = Csv.Row.to_list row in
          if not (Hashtbl.mem occs row) then
            Printf.sprintf
              "The row `%s` isn't supposed to be returned."
              (printer row)
            |> assert_failure;

          let count = Hashtbl.find occs row in
          Hashtbl.replace occs row (count + 1));

        occs |> Hashtbl.iter (fun row count ->
          if count = 0 then
            Printf.sprintf
              "The row `%s` is supposed to be returned."
              (printer row)
            |> assert_failure)

    | Ordered | Exact ->
        (* In both ordered and exact mode, we don't need
           to use a hashtable. We simply iterate over the
           rows and check them in order. *)
        let pos = ref 0 in
        ra |> Array.iter (fun row ->
          let rowa = Csv.Row.to_list row in
          let rowb = Csv.Row.to_list rb.(!pos) in

          if rowa <> rowb then
            Printf.sprintf
              "Row %d didn't match. Expected `%s` but got `%s`."
              !pos
              (printer rowa)
              (printer rowb)
            |> assert_failure;

          incr pos;
          (* If we don't want an exact match, there can be more
             than one occurence of the row in the output. *)
          if mode = Ordered then
            while !pos < Array.length rb &&
                  rowa = Csv.Row.to_list rb.(!pos) do
              incr pos;
            done)


(** make_case : string -> OUnit2.test_fun
    Builds a test case function for a given test
    file. This function will check whether the
    output of MiniSQL on the query from the file
    is the same as the expected output. *)
let make_case file =
  let rec parse_line line prev =
    if String.length line >= 1 &&
       String.sub line 0 1 = "[" then
      (prev,
       mode_of_line line,
       Csv.of_channel ~has_header:true file)
    else
      parse_line (input_line file) (prev ^ " " ^ line) in

  fun context ->
    let (query, mode, csv) =
      parse_line (input_line file) "" in

    let hash = Digest.string query |> Digest.to_hex in
    let temp_file = Filename.temp_file "minisql" hash in

    let params = {
      repl = ref false;
      out = ref temp_file;
      request = ref "";
      graphviz = ref ""} in

    let buffer = Lexing.from_string query in
    let ast = Common.parse_line ~with_endline:true buffer in
    Common.action params ast;

    open_in temp_file
    |> Csv.of_channel ~has_header:true
    |> compare_csvs mode csv


let () =
  let cases = list_files ()
    |> List.map (fun (name, file) -> name >:: make_case file) in
  let suite = "suite" >::: cases in
  run_test_tt_main suite
