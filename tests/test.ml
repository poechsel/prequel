open OUnit2
open Parser
open Lexer
open Ast


let parsing_test str ctx =
    let lexbuf = Lexing.from_string str in
    let _ = Parser.main Lexer.token lexbuf  in
    ()



let make_ast_atom x = AstCompOp(Eq, (None, x), (None, x))
let make_disj_atom x = DisjCompOp(Eq, (None, x), (None, x))

let test_disj a b ctx = assert_equal (Transformers.disjunction a) b 

(* Name the test cases and group them together *)
let suite =
"parsing">:::
[
  "test1">:: parsing_test "SELECT a FROM a WHERE a == a;"
 ]
;;

let disj_transform = 
  "disj" >:::
  [
    "disj01" >:: test_disj (AstSelect([], [], make_ast_atom "", None)) (AstSelect([], [], [[make_disj_atom ""]], None));
    "disj02" >:: test_disj (AstSelect([], [], AstBinOp(Or, make_ast_atom "a", make_ast_atom "b"), None))
      (AstSelect([], [], [[make_disj_atom "a"]; [make_disj_atom "b"]], None));
    "disj03" >:: test_disj (AstSelect([], [], AstBinOp(And, make_ast_atom "a", make_ast_atom "b"), None))
      (AstSelect([], [], [[make_disj_atom "a"; make_disj_atom "b"]], None));
    "disj04" >:: test_disj (AstSelect([], [], AstBinOp(And, AstBinOp(Or, make_ast_atom "a", make_ast_atom "b"), AstBinOp(Or, make_ast_atom "c", make_ast_atom "d")), None))
      (AstSelect([], [], [[make_disj_atom "a"; make_disj_atom "c"]; [make_disj_atom "b"; make_disj_atom "c"]; [make_disj_atom "a"; make_disj_atom "d"]; [make_disj_atom "b"; make_disj_atom "d"];], None));
    "disj05" >:: test_disj (AstSelect([], [], AstBinOp(And, AstBinOp(Or, make_ast_atom "a", make_ast_atom "b"), make_ast_atom "c"), None))
      (AstSelect([], [], [[make_disj_atom "a"; make_disj_atom "c"]; [make_disj_atom "b"; make_disj_atom "c"]], None));
      "disj06" >:: test_disj (AstSelect([], [AstSubQuery (AstSelect([], [], AstBinOp(And, AstBinOp(Or, make_ast_atom "a", make_ast_atom "b"), AstBinOp(Or, make_ast_atom "c", make_ast_atom "d")), None))], make_ast_atom "", None))
        (AstSelect([], [ AstSubQuery (AstSelect([], [], [[make_disj_atom "a"; make_disj_atom "c"]; [make_disj_atom "b"; make_disj_atom "c"]; [make_disj_atom "a"; make_disj_atom "d"]; [make_disj_atom "b"; make_disj_atom "d"];], None))], [[make_disj_atom ""]], None));
  ]

let () =
  run_test_tt_main suite;
  run_test_tt_main disj_transform
;;

