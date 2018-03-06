open Ast
open AlgebraTypes


let naive_compiler query =
  let rec compile_query query =
    match query with
    | AstSelect(attributes, tables, cond) ->
      let (AstTable x, _)::t = tables in
      let layer = AlgInput(x) in
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


  in compile_query query
