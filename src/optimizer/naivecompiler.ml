open Ast
open AlgebraTypes
open Arithmetics

let print_attribute at = 
  match at with
  | None, x -> print_string x
  | Some x, y -> Printf.printf "%s.%s" x y

let rec print_alg alg = 
  match alg with
  | AlgProjection (sub, headers) ->
    let _ = print_string "PROJ [" in
    let _ = List.iter (fun x -> let _ = print_attribute x in print_string " ") in
    let _ = print_string "]\n( " in
    let _ = print_alg sub in
    let _ = print_string "\n)\n" in ()
  | AlgSelect (sub, expr ) ->
    let _ = print_string "SELECT [" in
    let _ = print_string "<expr> ]" in
    let _ = print_string "\n( " in
    let _ = print_alg sub in
    let _ = print_string "\n)\n" in ()
  | AlgProduct (a, b) ->
    let _ = print_string "PRODUCT " in
    let _ = print_string "\n( " in
    let _ = print_alg a in
    let _ = print_string "\n)\n" in
    let _ = print_string "\n( " in
    let _ = print_alg b in
    let _ = print_string "\n)\n" in ()
  | AlgInput(s) -> print_string "input"
  | AlgRenameTable (sub, what) -> 
    let _ = print_string "RENAME [" in
    let _ = print_string what in
    let _ = print_string "]\n( " in
    let _ = print_alg sub in
    let _ = print_string "\n)\n" in ()


let merge_list fct l = 
  if List.length l = 1 then 
    List.hd l
  else 
    List.fold_left (fun a b ->
        fct a b
      ) (List.hd l) (List.tl l)

let naive_compiler query =
  let rec compile_query query =
    match query with
    | AstSelect(attributes, tables, cond) ->
      let layer = 
        List.map compile_relation_renamed tables 
        |> merge_list (fun a b -> AlgProduct(a, b)) in
      (*
      let layer = match cond with
        | None -> layer
        | Some expr -> AlgSelect(layer, alg_expr_of_ast_expr expr)
      in*)
      let layer = compile_where_clause layer cond in
      let layer = match attributes with
        | [] -> layer
        | _ -> AlgProjection(layer
                            , List.map fst attributes)
      in layer
    | _ -> failwith "not implemented"

  and compile_relation_renamed rel =
    match rel with
    | x, y-> 
      AlgRenameTable(compile_relation x, y)

  and compile_relation rel = 
    match rel with
    | AstTable x ->
      AlgInput x
    | AstSubQuery q ->
      compile_query q

  and compile_where_clause source cond =
    match cond with
    | None -> source
    | Some (pure, sub) -> 
      let convert_and and_expr = 
        List.map alg_expr_of_ast_expr and_expr
        |> merge_list (fun a b -> AlgBinOp(And, a, b)) 
      in 
      let layer = if List.length pure > 0 then
          let pure = List.map convert_and pure
                     |> merge_list (fun a b -> AlgBinOp(And, a, b))
          in 
          AlgSelect(source, pure) 
        else source
      in 
      if List.length sub > 0 then 
        let convert_and_in expr = List.fold_left (fun previous current ->
            match current with
            | DisjIn (expr, (AstSelect(attributes', tables', cond') as where)) ->
              let tables'' = 
                previous :: List.map compile_relation_renamed tables' 
              (* TODO: make sur to rename before doing the join *)
                |> merge_list (fun a b -> AlgProduct(a, b)) in
              let cond' = match cond' with
                | None -> Some ([[current]], [[]])
                | Some (a, b) -> Some (List.map (fun x -> DisjCompOp(Eq, expr, AstAtom(Attribute (Some "s", "dpt"))) :: x) a, b)
              in 
              let layer = compile_where_clause tables'' cond' in
              (* need to work on projection : we must project on everything but the attributes in attributes *)
              let layer = match attributes' with
                | [] -> layer
                | _ -> layer
            in layer 
            | DisjNotIn _ -> failwith "not implemented"
            | y -> AlgSelect(layer, alg_expr_of_ast_expr y)
          ) layer expr
        in List.map convert_and_in sub
           |> merge_list (fun a b -> AlgUnion(a, b))
      else layer


  in compile_query query
