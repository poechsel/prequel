open Ast

(* simple transformation to test if everything is working correctly *)
let rec identity x =
  x

let rec disjunction (query : cond query) : disj list list query =
  (* Convert a query where the conditions can have any form
     to a query where conditions are in disjunctive form 
    
     @Param query: the query of type cond query
  *)
  let rec disjunction_query query =
    match query with
    | AstMinus(a, b) ->
      AstMinus(disjunction_query a
              , disjunction_query b)
    | AstUnion(a, b) ->
      AstUnion(disjunction_query a
              , disjunction_query b)
    | AstSelect(attrs, rels, cond, group_cond) ->
      AstSelect(attrs
               , List.map (fun x -> disjunction_relation x) rels
               , disjunction_cond cond
               , (match group_cond with
                  | None -> None
                  | Some x -> Some (disjunction_cond x)
                 )
               )
  and disjunction_relation rel =
    begin
      match rel with
      | AstTable y -> 
        AstTable y
      | AstSubQuery y ->
        AstSubQuery (disjunction_query y)
    end
  and disjunction_cond cond =
    match cond with
    | AstBinOp(Or, a, b) ->
      (disjunction_cond a) @ (disjunction_cond b)
    | AstBinOp(And, a, b) ->
      let t1 = disjunction_cond a in
      let t2 = disjunction_cond b in
      List.concat (
        List.map ( fun x ->
            List.map (fun y ->
                y @ x
              ) t1
        ) t2
      ) 
    | AstCompOp(op, a, b) ->
      [[ AstCompOp(op, a, b) ]]
    | AstIn(a, b) ->
      [[ AstIn(a, disjunction_query b) ]]
    | AstNotIn(a, b) ->
      [[ AstNotIn(a, disjunction_query b) ]]
  in
  disjunction_query query

