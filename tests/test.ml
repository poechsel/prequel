open OUnit2
open Parser
open Lexer


let parsing_test str ctx =
    let lexbuf = Lexing.from_string str in
    let _ = Parser.main Lexer.token lexbuf  in
    ()


(* Name the test cases and group them together *)
let suite =
"parsing">:::
[
  "test1">:: parsing_test "SELECT a FROM a WHERE a == a;"
 ]
;;

let () =
  run_test_tt_main suite
;;
