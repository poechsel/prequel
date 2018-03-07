open Ast
open AlgebraTypes

let naive_compiler query =
  let rec compile_query query =
    match query with
    | AstSelect(attributes, tables, cond) ->
      let layer = List.fold_left (fun a b ->
            AlgProduct(a, compile_relation_renamed b)
        ) (compile_relation_renamed (List.hd tables)) (List.tl tables) in
      let layer = match cond with
        | None -> layer
        | Some expr -> AlgSelect(layer, alg_expr_of_ast_expr expr)
      in 
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


  in compile_query query
