open AlgebraTypes
open Ast


let string_of_op op = 
  match op with
  | Eq -> "="
  | Neq -> "!="
  | Leq -> "<="
  | Geq -> ">="
  | Lt -> "<"
  | Gt -> ">"
  | And -> "&&"
  | Or -> "||"
  | Add -> "+"
  | Sub -> "-"
  | Times -> "*"
  | Div -> "/"

let string_of_header header = 
  let prefix = match fst header with
    | "" -> ""
    | x -> x ^ "."
  in prefix ^ snd header

let string_of_atom atom = 
  match atom with 
  | Attribute x -> string_of_header x
  | Number x -> string_of_int x
  | String s -> "\"" ^ s  ^ "\""

let rec string_of_alg_expr expr = 
  match expr with
  | AlgAtom at -> string_of_atom at
  | AlgBinOp (op, a, b) ->
    Printf.sprintf "(%s %s %s)" (string_of_alg_expr a) (string_of_op op) (string_of_alg_expr b)

let graphviz_instrs_of_algebra alg =
  let uid = ref 0 in
  let node = format_of_string "%d [label = \"%s (%d)\"]" in
  let edge = format_of_string "%d -> %d" in
  let edge_label = format_of_string "%d -> %d [label = \"%s\"]" in
  let rec conv_alg alg =
    match alg with
    | AlgUnion(u, a, b) ->
      let a_lbl, a_str = conv_alg a in
      let b_lbl, b_str = conv_alg b in
      let _ = incr uid in
      !uid, a_str 
            @ b_str 
            @ [Printf.sprintf node !uid "union" u;
               Printf.sprintf edge !uid a_lbl;
               Printf.sprintf edge !uid b_lbl]
    | AlgMinus(u, a, b) ->
      let a_lbl, a_str = conv_alg a in
      let b_lbl, b_str = conv_alg b in
      let _ = incr uid in
      !uid, a_str 
            @ b_str 
            @ [Printf.sprintf node !uid "minus" u;
               Printf.sprintf edge !uid a_lbl;
               Printf.sprintf edge !uid b_lbl]

    | AlgProduct(u, a, b) ->
      let a_lbl, a_str = conv_alg a in
      let b_lbl, b_str = conv_alg b in
      let _ = incr uid in
      !uid, a_str 
            @ b_str 
            @ [Printf.sprintf node !uid "product" u;
               Printf.sprintf edge !uid a_lbl;
               Printf.sprintf edge !uid b_lbl]

    | AlgJoin(u, (a, ea), (b, eb)) ->
      let a_lbl, a_str = conv_alg a in
      let b_lbl, b_str = conv_alg b in
      let _ = incr uid in
      !uid, a_str 
            @ b_str 
            @ [Printf.sprintf node !uid "join" u;
               Printf.sprintf edge_label !uid a_lbl (string_of_alg_expr ea);
               Printf.sprintf edge_label !uid b_lbl (string_of_alg_expr eb)]


    | AlgInput(u, name) ->
      let _ = incr uid in
      !uid, [Printf.sprintf node !uid name u]

    | AlgProjection(u, a, headers) ->
      let a_lbl, a_str = conv_alg a in
      let _ = incr uid in
      !uid, a_str 
            @ [Printf.sprintf node !uid "projection" u;
               Printf.sprintf edge_label !uid a_lbl 
                 (Utils.array_concat "; " (Array.map string_of_header headers))]


    | AlgSelect(u, a, expr) ->
      let a_lbl, a_str = conv_alg a in
      let _ = incr uid in
      !uid, a_str 
            @ [Printf.sprintf node !uid "selection" u;
               Printf.sprintf edge_label !uid a_lbl (string_of_alg_expr expr)]

    | AlgRename (u, a, rename) ->
      let a_lbl, a_str = conv_alg a in
      let _ = incr uid in
      !uid, a_str 
            @ [Printf.sprintf node !uid "rename" u;
               Printf.sprintf edge_label !uid a_lbl (String.concat ", " (List.map (fun (a, b) -> string_of_header a ^ "=" ^ string_of_header b) rename))]


  in conv_alg alg


let graphviz_to_channel channel graphviz_instructions =
  let str = String.concat ";\n" graphviz_instructions in
  let _ = Printf.fprintf channel "digraph G{\n%s\n}" str in
  ()


let graphviz_of_algebra channel alg = 
  snd (graphviz_instrs_of_algebra alg)
  |> graphviz_to_channel channel
