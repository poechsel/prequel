open Ast
open AlgebraTypes
open Arithmetics

let naive_compiler query =
  let rec compile_query query =
    match query with
    | AstSelect(attributes, tables, cond) ->
      let layer = List.fold_left (fun a b ->
            AlgProduct(a, compile_relation_renamed b)
        ) (compile_relation_renamed (List.hd tables)) (List.tl tables) in
      (*
      let layer = match cond with
        | None -> layer
        | Some expr -> AlgSelect(layer, alg_expr_of_ast_expr expr)
      in*)
      let layer = compile_where_clause layer cond in
      let layer = match attributes with
        | [] -> layer
        | _ -> AlgProjection(layer
                            , List.map (fun i -> (fst i)) attributes)
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
      let convert_and and_expr = List.fold_left (fun a b ->
            AlgBinOp(And, a, alg_expr_of_ast_expr b)
        ) (alg_expr_of_ast_expr @@ List.hd and_expr) (List.tl and_expr) 
      in
      let pure = List.fold_left (fun a b ->
          AlgBinOp(Or, a, convert_and b))
          (convert_and @@ List.hd pure) (List.tl pure)
      in AlgSelect(source, pure)


  in compile_query query
