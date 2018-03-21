open Common

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
