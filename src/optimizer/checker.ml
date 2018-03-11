open Ast
open Utils


let headers_union a b =
  (* compute the union of a b.
     Works only if a contains the same headers as b *)
  if List.for_all2 (=) (List.sort (Pervasives.compare) a) (List.sort (Pervasives.compare) b) then
    a
  else 
    failwith "a et b différents"

let headers_join a b = 
  let temp = List.merge (Pervasives.compare) (List.sort (Pervasives.compare) a) (List.sort (Pervasives.compare) b) in
  let temp2 = List.sort_uniq (Pervasives.compare) temp in
  if List.length temp != List.length temp2 then
    failwith "their is one common element"
  else 
    a @ b

let check_coherence query =
  let rec check_query headers query =
    let c_qu = check_query headers in
    match query with
    | AstMinus(a, b) ->
      let a_h, a' = c_qu a in
      let b_h, b' = c_qu b in
      headers_union a_h b_h, AstMinus(a', b')
    | AstUnion(a, b) ->
      let a_h, a' = c_qu a in
      let b_h, b' = c_qu b in
      headers_union a_h b_h, AstUnion(a', b')

    | AstSelect(attributes, tables, selector) ->
      let headers_collection, tables = 
        let h, t = List.map (check_relation headers) tables 
                   |> List.split 
        in merge_list headers_join h, List.fold_left (fun a b -> b::a ) [] t
      in let headers_collection = headers_join headers_collection headers
      in let selector = match selector with
        | None -> None
        | Some x -> Some (check_cond headers_collection x)
      in let headers, attributes = match attributes with
          | [] -> headers_collection, List.map (fun a ->a, None) headers_collection
          | x -> List.map (fun ((a, b), c) -> 
              match c with 
              | None -> (a, b) 
              | Some x -> (a, x)
            ) x, x
      in headers, AstSelect(attributes, tables, selector)

  and check_relation headers relation = 
    let headers, ast =
      match fst relation with
    | AstSubQuery x ->
      let a, b = check_query headers x in
      a, AstSubQuery b
    | AstTable name ->
      let oc = open_in name in
      let csv = Csv.of_channel ?has_header:(Some true) oc in
      let headers = List.map (fun i -> Some(name), i) @@ Csv.Rows.header csv in
      headers, AstTable name
    in List.map (fun (_, c) -> Some (snd relation), c) headers, 
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
        in (*raise error here*)
        AstIn(c_e e, sub)
      | AstNotIn(e, sub) ->
        let h, sub = check_query headers sub
        in (*raise error here*)
        AstNotIn(c_e e, sub)

    and check_expr headers expr = 
      let c_e = check_expr headers in
      match expr with
      | AstExprOp(op, a, b) ->
        AstExprOp(op, c_e a, c_e b)
      | AstAtom(atom) ->
        AstAtom(check_atom headers atom)

    and check_atom headers atom =
      match atom with
      | Attribute attr ->
        (* send error *)
        Attribute attr
      | x -> x




    in snd @@ check_query [] query
