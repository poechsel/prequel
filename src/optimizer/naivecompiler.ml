open Ast
open AlgebraTypes

let naive_compiler query =
  let rec compile_query query =
    match query with
    | AstSelect(attributes, tables, None) ->
      let attributes = List.map (fun i -> (fst i)) attributes in
      let (AstTable x, _)::t = tables in
      AlgProjection(AlgInput(x), attributes)


  in compile_query query
