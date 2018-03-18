open Ast
open AlgebraTypes
open Arithmetics
open Utils

let print_attribute at = 
  match at with
  | None, x -> print_string x
  | Some x, y -> Printf.printf "%s.%s" x y


let rec add_table_to_query query table = 
  match query with
  | AstSelect(at, tables, where) ->
    AstSelect(at, table::tables, where)
  | AstUnion(a, b) ->
    AstUnion(add_table_to_query a table, add_table_to_query b table)
  | AstMinus(a, b) ->
    AstMinus(add_table_to_query a table, add_table_to_query b table)

let rec add_condition_to_query query cond = 
  match query with
  | AstSelect(at, tables, where) ->
    AstSelect(at, tables, begin match where with
        | None -> Some ([[cond]], [])
      | Some (x, y) -> Some (List.map (fun x -> cond::x) x, List.map (fun x -> cond :: x) y)
    end)
  | AstUnion(a, b) ->
    AstUnion(add_condition_to_query a cond, add_condition_to_query b cond)
  | AstMinus(a, b) ->
    AstMinus(add_condition_to_query a cond, add_condition_to_query b cond)

let rec get_attributes_query query =
  match query with
  | AstSelect(at, _, _) ->
    at
  | AstUnion(a, b) | AstMinus(a, b) ->
    get_attributes_query a

let compile query =
  (* compile a given query
     The query MUST be in disjunctive form
     Return an algebra *)
  let rec compile_query ?(project=true) query =
    match query with
    | AstMinus (a, b) ->
      AlgMinus(new_uid (), compile_query ~project:project a, compile_query ~project:project b)

    | AstUnion (a, b) ->
      AlgUnion(new_uid (), compile_query ~project:project a, compile_query ~project:project b)

    | AstSelect(attributes, tables, cond) ->
      let layer = 
        List.map compile_relation_renamed tables 
        |> merge_list (fun a b -> AlgProduct(new_uid(), a, b)) in
      let layer = compile_where_clause layer cond in
      let layer = match attributes with
        | [] -> layer
        | _ when project -> AlgProjection(new_uid(), layer, List.map fst attributes |> Array.of_list)
        | _ -> layer
      in layer

  and compile_relation_renamed rel =
    match rel with
    | x, ""-> 
      compile_relation x
    | x, y-> 
      AlgRenameTable(new_uid(), compile_relation x, y)
    

  and compile_relation rel = 
    match rel with
    | AstTable x ->
      AlgInput (new_uid (), x)
    | AstSubQuery q ->
      compile_query q
    | AstCompiled q ->
      q

  and compile_where_clause source cond =
    match cond with
    | None -> source
    | Some (disjunctive_clauses) -> 
      let layer = source in
      (* to compile the part composed of "subqueries", we use to remarks:
         - a or can be converted with a union
         - select_cond1(select_cond2(...)) = select_(cond1/\cond2)(...)
            This remark could be usefull for optimisations
      *)
      if List.length disjunctive_clauses > 0 then 
        let attr_from_select s = match s with 
          | (a, b), None 
          | (a, _), Some b -> Attribute (a, b) 
        in
        let convert_and_in expr = List.fold_left (fun previous current ->
            match current with
            | DisjNotIn (expr, query) -> 
              let at = List.hd @@ get_attributes_query query in
              let query' = add_table_to_query query (AstCompiled (previous), "") in
              let attribute = attr_from_select at in
              AlgSelect(new_uid(), compile_query ~project:false query',
                        alg_expr_of_ast_expr (DisjCompOp(Neq, expr, AstAtom(attribute)))) 
            | DisjIn(expr, query) ->
              let at = List.hd @@ get_attributes_query query in
              let query' = add_table_to_query query (AstCompiled (previous), "") in
              let attribute = attr_from_select at in
              AlgSelect(new_uid(), compile_query ~project:false query',
                        alg_expr_of_ast_expr (DisjCompOp(Eq, expr, AstAtom(attribute)))) 
            (* let t = (DisjCompOp(Eq, expr, AstAtom(attribute))) in
               compile_query ~project:false (add_condition_to_query query' t) *)
            | y -> AlgSelect(new_uid(), previous, alg_expr_of_ast_expr y)
          ) layer expr
        in List.map convert_and_in disjunctive_clauses
           |> merge_list (fun a b -> AlgUnion(new_uid (), a, b))
      else layer


  in compile_query query
