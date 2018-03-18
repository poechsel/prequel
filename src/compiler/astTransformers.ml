open Ast

(* simple transformation to test if everything is working correctly *)

let cartesian f l l' = 
  List.concat (List.map (fun e -> List.map (fun e' -> f e e') l') l)

let rec identity x =
  x
let rec disjunction (query : ('b cond, 'b) query) : ('b disj list list, 'b) query =
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
      | AstCompiled y ->
        AstCompiled y
    end, snd rel
  and disjunction_cond cond =
    match cond with
    | AstBinOp(Or, a, b) ->
      let t1 = disjunction_cond a in
      let t2 = disjunction_cond b in
      t1 @ t2
    | AstBinOp(And, a, b) ->
      let t1 = disjunction_cond a in
      let t2 = disjunction_cond b in
      cartesian (@) t1 t2
    | AstCompOp(op, a, b) ->
      [ [ DisjCompOp(op, a, b) ] ]
    | AstIn(a, b) ->
      [ [ DisjIn(a, disjunction_query b) ] ]
    | AstNotIn(a, b) ->
      [ [ DisjNotIn(a, disjunction_query b) ] ]
    | AstBinOp(_, a, b) ->
      failwith "unexpected operator during transformation"
  in
  disjunction_query query
