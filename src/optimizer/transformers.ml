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
    | AstSelect(attrs, rels, None) ->
      AstSelect(attrs
               , List.map (fun x -> disjunction_relation x) rels
               , None 
               )
    | AstSelect(attrs, rels, Some cond) ->
      AstSelect(attrs
               , List.map (fun x -> disjunction_relation x) rels
               , Some (disjunction_cond cond)
               )
  and disjunction_relation rel =
    begin
      match (fst rel) with
      | AstTable y -> 
        AstTable y
      | AstSubQuery y ->
        AstSubQuery (disjunction_query y)
    end, snd rel
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
      [[ DisjCompOp(op, a, b) ]]
    | AstIn(a, b) ->
      [[ DisjIn(a, disjunction_query b) ]]
    | AstNotIn(a, b) ->
      [[ DisjNotIn(a, disjunction_query b) ]]
  in
  disjunction_query query

let rec remove_or query = 
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
    | AstSelect(attrs, rels, Some conds) ->
      let conds = disjunction_cond conds in
      let rels = List.map (fun x -> disjunction_relation x) rels in
      List.fold_left (fun a b ->
            AstUnion(a, AstSelect(attrs, rels, Some b))
        ) (AstSelect(attrs, rels, Some (List.hd conds)))
        (List.tl conds)
    | AstSelect(attrs, rels, None) ->
      let rels = List.map (fun x -> disjunction_relation x) rels in
      AstSelect(attrs, rels, None)
  and disjunction_relation rel =
    begin
      match fst rel with
      | AstTable y -> 
        AstTable y
      | AstSubQuery y ->
        AstSubQuery (disjunction_query y)
    end, snd rel
  and disjunction_cond cond =
     List.map (fun x ->
        List.map (fun y -> 
             match y with
             | DisjCompOp(op, a, b) ->
               DisjNOCompOp(op, a, b)
             | DisjIn(a, b) -> 
               DisjNOIn(a, disjunction_query b)
             | DisjNotIn(a, b) -> 
               DisjNONotIn(a, disjunction_query b)
           ) x
       ) cond
  in 
  disjunction_query query

