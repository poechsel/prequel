open Ast
open Utils

module Headers = struct
  type 'a t = 'a list

  let union a b = 
  (* compute the union of a b.
     Works only if a contains the same headers as b *)
    if List.for_all2 (=) (List.sort (Pervasives.compare) a) (List.sort (Pervasives.compare) b) then
      a
    else 
      raise (Errors.SemanticError "Some headers are different")

  let join a b = 
    a @ b

  let has_duplicate l = 
    List.length @@ List.sort_uniq (Pervasives.compare) l != List.length l

end 



let check_coherence query =
  (* Check if a given query seems 'coherent'. That means: 
      - every input csv exists
      - the tables we are trying to access exists
      - we do not have conflicts for row names
      - in and not in subqueries returns only one row
  *)
  let uid_expr = ref 0 in
  let new_uid_expr () = incr uid_expr; !uid_expr in
  let rec check_query headers query =
    let c_qu = check_query headers in
    match query with
    | AstMinus(a, b) ->
      let a_h, a' = c_qu a in
      let b_h, b' = c_qu b in
      Headers.union a_h b_h, AstMinus(a', b')
    | AstUnion(a, b) ->
      let a_h, a' = c_qu a in
      let b_h, b' = c_qu b in
      Headers.union a_h b_h, AstUnion(a', b')

    | AstSelect(attributes, tables, selector) ->
      let headers_tables, tables = 
        let headers, tables = tables
                              |> List.map (check_relation headers)
                              |> List.split 
        in merge_list Headers.join headers
         , tables
      in let headers = Headers.join headers_tables headers
      in let selector = Utils.option_map (check_cond headers) selector
      in let attributes = List.map (check_select_attribute headers) attributes
      in let headers, attributes = 
           if attributes = [] then 
             headers, List.map (fun at -> AstSeAttribute at) headers
           else 
             attributes 
             |> List.map (fun attribute ->
                 match attribute with
                 | AstSeRenamed(x, new_name) ->
                   ("", new_name), attribute
                 | AstSeAttribute(at) ->
                   at, attribute
                 | AstSeExpr(expr) ->
                   let new_name = new_uid_expr () |> string_of_int in
                   ("", new_name), AstSeRenamed(attribute, new_name)
               )
             |> List.split

      in headers, AstSelect(attributes, tables, selector)

  and check_relation headers relation = 
    let headers, ast =
      match fst relation with
      | AstSubQuery x ->
        let a, b = check_query headers x in
        a, AstSubQuery b
      | AstTable name ->
        let headers = 
          try
            InputCachedFile.get_headers name
            |> Array.to_list
          with e ->
            raise (Errors.SemanticError (Printf.sprintf "error: file \"%s\" doesn't exists" name)) 
        in
        headers, AstTable name
      | AstCompiled x ->
        [], AstCompiled x
    in List.map (fun (_, c) -> snd relation, c) headers, 
       (ast, snd relation)

  and check_cond headers cond = 
    let c_c = check_cond headers in 
    let c_e = check_expr headers in
    match cond with
    | AstBinOp(op, a, b) ->
      AstBinOp(op, c_c a, c_c b)
    | AstCompOp(op, a, b) ->
      AstCompOp(op, c_e a, c_e b)
    | AstIn(e, sub) ->
      let h, sub = check_query headers sub
      in if List.length h != 1 then
        raise (Errors.SemanticError "the subquery inside a 'in' must have only one row")
      else 
        AstIn(c_e e, sub)
    | AstNotIn(e, sub) ->
      let h, sub = check_query headers sub
      in if List.length h != 1 then
        raise (Errors.SemanticError "the subquery inside a 'not in' must have only one row")
      else 
        AstNotIn(c_e e, sub)

  and check_expr headers expr = 
    let c_e = check_expr headers in
    match expr with
    | AstExprOp(op, a, b) ->
      AstExprOp(op, c_e a, c_e b)
    | AstAtom(atom) ->
      AstAtom(check_atom headers atom)

  and check_select_attribute headers attribute = 
    match attribute with
    | AstSeAttribute (table, at) ->
      let real =  
        headers 
        |> Utils.find_first (fun (x, y) -> 
            if table = "" then 
              y = at 
            else 
              (x = table) && (y = at)
          )
      in begin
        match real  with
        | None ->
          raise (Errors.SemanticError (""))
        | Some x ->
          AstSeAttribute x
      end 
    | AstSeRenamed(s, n) ->
      AstSeRenamed(check_select_attribute headers s, n)
    | AstSeExpr(expr) ->
      AstSeExpr(check_expr headers expr)
        

  and check_atom headers atom =
    match atom with
    | Attribute (table, at) ->
      let real =  
        headers 
        |> Utils.find_first (fun (x, y) -> 
            if table = "" then 
              y = at 
            else 
              (x = table) && (y = at)
          )
      in begin
        match real  with
        | None ->
          raise (Errors.SemanticError (Printf.sprintf "Attribute \"%s\" doesn't exists" (Debug.string_of_atom atom)))
        | Some x ->
          Attribute x
      end 
    | x -> x




  in snd @@ check_query [] query


module Env = Map.Make(struct
    type t = string
    let compare = Pervasives.compare
  end)

let rename_tables query =
  (* Rename every table to a uid
     This will allow us, during compilation, to 
     avoid having to deal with renaming to avoid conflicts 
  *)
  let uid = ref 0 in
  let ren_attribute env (m, a) =
    if Env.mem m env then
      (Env.find m env, a)
    else 
      (m, a)
  in let rec ren_query env query = 
       match query with
       | AstSelect (attributes, relations, where) ->
         let env' = env in
         let env'' = List.fold_left (fun previous (_, c) -> incr uid; Env.add c (string_of_int !uid) previous) env' relations in
         let relations = List.map (fun (rel, c) ->
             (match rel with
              | AstSubQuery q -> AstSubQuery(ren_query env q)
              | _ -> rel
             ), Env.find c env''
           ) relations
         in let attributes = List.map (fun attribute ->
             ren_attribute_select env'' attribute
           ) attributes
         in let where = Utils.option_map (ren_cond env'') where
         in AstSelect(attributes, relations, where)
       | AstUnion(a, b) ->
         AstUnion(ren_query env a, ren_query env b)
       | AstMinus(a, b) ->
         AstMinus(ren_query env a, ren_query env b)

  and ren_attribute_select env attribute =
    match attribute with
    | AstSeRenamed(x, a) ->
      AstSeRenamed(ren_attribute_select env x, a)
    | AstSeAttribute (a, b) ->
      AstSeAttribute (ren_attribute env (a, b))
    | AstSeExpr(expr) ->
      AstSeExpr(ren_expr env expr)

  and ren_cond env cond = 
    match cond with
    | AstBinOp(op, a, b) ->
      AstBinOp(op, ren_cond env a, ren_cond env b)
    | AstCompOp(op, a, b) ->
      AstCompOp(op, ren_expr env a, ren_expr env b)
    | AstIn(expr, query) ->
      AstIn(ren_expr env expr, ren_query env query)
    | AstNotIn(expr, query) ->
      AstNotIn(ren_expr env expr, ren_query env query)

  and ren_expr env expr =
    match expr with
    | AstExprOp(op, a, b) ->
      AstExprOp(op, ren_expr env a, ren_expr env b)
    | AstAtom x ->
      AstAtom(ren_atom env x)

  and ren_atom env atom = 
    match atom with
    | Attribute (x) ->
      Attribute(ren_attribute env x)
    | x -> x

  in ren_query (Env.empty) query
