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


let list_find_and_remove l fct = 
  let rec aux l acc found = 
    match l with
    | [] -> found, List.rev acc
    | x::tl when fct x ->
      if found = None then
        aux tl acc (Some x)
      else 
        aux tl (x::acc) found
    | x::tl ->
      aux tl (x::acc) found
  in aux l [] None

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
      let product_terms = List.map compile_relation_renamed tables in
      let layer = compile_where_clause product_terms cond in
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
      let compiled_x = compile_relation x in
      let headers = MetaQuery.get_headers compiled_x in
      let rename_op = 
        Array.map (fun (a, b) -> (a, b), (y, b)) headers
        |> Array.to_list
      in 
      AlgRename(new_uid(), compiled_x, rename_op)
    

  and compile_relation rel = 
    match rel with
    | AstTable x ->
      AlgInput (new_uid (), x)
    | AstSubQuery q ->
      compile_query q
    | AstCompiled q ->
      q

  and compile_where_clause product_terms cond =
    let compute_final_term product = 
      merge_list (fun a b -> AlgProduct(new_uid (), a, b)) product
    in 
    match cond with
    | None -> 
      compute_final_term product_terms
    | Some (disjunctive_clauses) -> 
      let layer = compute_final_term product_terms in
      (* to compile the part composed of "subqueries", we use to remarks:
         - a or can be converted with a union
         - select_cond1(select_cond2(...)) = select_(cond1/\cond2)(...)
            This remark could be usefull for optimisations
      *)
        let attr_from_select s = match s with 
          | (a, b), None 
          | (a, _), Some b -> Attribute (a, b) 
        in
        let rec step_once_for_joins and_exprs acc tables = 
          match and_exprs with
          | [] -> List.rev acc, tables
          | (DisjCompOp(Eq, lhs, rhs) as x)::tl ->
            let lhs = alg_expr_of_ast_expr lhs in
            let rhs = alg_expr_of_ast_expr rhs in
            let attrs_lhs = attributes_of_condition lhs in
            let attrs_rhs = attributes_of_condition rhs in
            if List.length attrs_lhs = 1 && List.length attrs_rhs = 1 && fst @@ List.hd attrs_lhs <> fst @@ List.hd attrs_rhs then 
              let expr_lhs = List.hd attrs_lhs in
              let expr_rhs = List.hd attrs_rhs in
              let table_lhs, tables' = list_find_and_remove tables (fun x -> Array.exists ((=) expr_lhs) @@ MetaQuery.get_headers x) in
              let table_rhs, tables'' = list_find_and_remove tables' (fun x -> Array.exists ((=) expr_rhs) @@ MetaQuery.get_headers x) in
              match table_lhs, table_rhs with
              | Some x, Some y ->
                let _ = print_string "yes\n" in
                let _ = Printf.printf "%d %d %d \n" (List.length tables) (List.length tables') (List.length tables'') in
                let tables = (AlgJoin(new_uid(), (x, lhs), (y, rhs)) :: tables'') in
                step_once_for_joins tl acc tables
              | _, _ ->
                step_once_for_joins tl (x::acc) tables
            else 
              let a, b = step_once_for_joins tl (x::acc) tables in
              x::a, b

          | x::tl ->
              let a, b = step_once_for_joins tl (x::acc) tables in
              x::a, b

        in 
        let rec repeat_joins_steps and_exprs previous tables =
          let _ = Printf.printf "-> step %d\n" (List.length tables) in
          if List.length previous = List.length and_exprs then
            and_exprs, tables
          else 
            let and_exprs', tables' = step_once_for_joins and_exprs [] tables in
            repeat_joins_steps and_exprs' and_exprs tables'

                
        in 
        let convert_and_in and_exprs = 
          let and_exprs, product_terms = repeat_joins_steps and_exprs [] product_terms in
          let layer = compute_final_term product_terms in
          List.fold_left (fun previous current ->
            match current with
            | DisjNotIn (expr, query) -> 
              let at = List.hd @@ get_attributes_query query in
              let query' = add_table_to_query query (AstCompiled (previous), "") in
              let attribute = attr_from_select at in
              AlgSelect(new_uid(), compile_query ~project:false query',
                        alg_expr_of_ast_cond (DisjCompOp(Neq, expr, AstAtom(attribute)))) 
            | DisjIn(expr, query) ->
              let at = List.hd @@ get_attributes_query query in
              let query' = add_table_to_query query (AstCompiled (previous), "") in
              let attribute = attr_from_select at in
              AlgSelect(new_uid(), compile_query ~project:false query',
                        alg_expr_of_ast_cond (DisjCompOp(Eq, expr, AstAtom(attribute)))) 
            (* let t = (DisjCompOp(Eq, expr, AstAtom(attribute))) in
               compile_query ~project:false (add_condition_to_query query' t) *)
            | y -> AlgSelect(new_uid(), previous, alg_expr_of_ast_cond y)
          ) layer and_exprs

        in List.map convert_and_in disjunctive_clauses
           |> merge_list (fun a b -> AlgUnion(new_uid (), a, b))


  in compile_query query
