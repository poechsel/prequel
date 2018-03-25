open AlgebraTypes

let exclusive_join l1 l2 =
  let rec aux l acc = 
    match l with
    | [] -> acc
    | x::y::tl when x = y -> aux tl (x::acc)
    | x::tl -> aux tl acc
  in 
  let l1 = List.sort_uniq Pervasives.compare l1 in
  let l2 = List.sort_uniq Pervasives.compare l2 in
  let l = List.merge Pervasives.compare l1 l2 in
  aux l []


module SetAttributes = Set.Make (struct
    type t = AlgebraTypes.header
    let compare = Pervasives.compare
  end )

let push_down_select query = 
  let tbl = Hashtbl.create 10 in
  let _ = MetaQuery.get_headers ~f:(fun x y -> Hashtbl.add tbl x y) query in
  let get_headers query =
    Hashtbl.find tbl (MetaQuery.get_uid_from_alg query)
    |> Array.to_list
    |> SetAttributes.of_list 
  in 
  let rec push_down can_be_pushed query =
    let analyze_sub query =
      let header = get_headers query in
      let push, stay =
        can_be_pushed 
        |> List.partition (fun x -> SetAttributes.subset (snd x) header) 
      in stay, push_down push query
    in 
    let to_insert, req = 
      match query with
      | AlgUnion(u, a, b) ->
        let i1, a' = analyze_sub a in 
        let i2, b' = analyze_sub b in
        exclusive_join i1 i2, AlgUnion(u, a', b')

      | AlgMinus(u, a, b) ->
        let i1, a' = analyze_sub a in 
        let i2, b' = analyze_sub b in
        exclusive_join i1 i2, AlgMinus(u, a', b')
      
      | AlgProduct(u, a, b) ->
        let i1, a' = analyze_sub a in 
        let i2, b' = analyze_sub b in
        exclusive_join i1 i2, AlgProduct(u, a', b')
       
      | AlgJoin(u, (a, expr_a), (b, expr_b)) ->
        let i1, a' = analyze_sub a in 
        let i2, b' = analyze_sub b in
        exclusive_join i1 i2, AlgJoin(u, (a', expr_a), (b', expr_b))

      | AlgProjection(u, a, headers) ->
        let i, a' = analyze_sub a in
        i, AlgProjection(u, a', headers)

      | AlgRename(u, a, name) ->
        let i, a' = analyze_sub a in
        i, AlgRename(u, a', name)

      | AlgAddColumn(u, a, expr, name) ->
        let i, a' = analyze_sub a in
        i, AlgAddColumn(u, a', expr, name)

      | AlgSelect(u, a, filter) ->
        let attrs = attributes_of_condition filter |> SetAttributes.of_list in
        let a' = push_down ((filter, attrs) :: can_be_pushed) a in
        [], a'

      | AlgInput(u, str) ->
        can_be_pushed, query

      | AlgOrder(u, a, criterion) ->
        let i, a' = analyze_sub a in
        i, AlgOrder(u, a', criterion)

    in List.fold_left (fun a (cond, _) -> AlgSelect(AlgebraTypes.new_uid (), a, cond))
         req
         to_insert
  in push_down [] query




(* SELECT compressor *)

let rec select_compressor alg =
  match alg with 
  | AlgSelect(_, AlgSelect(_, sub, e1), e2) ->
    select_compressor (AlgSelect(AlgebraTypes.new_uid(), sub, AlgBinOp(Ast.And, e2, e1)))
  | AlgUnion(u, a, b) ->
    AlgUnion(u, (select_compressor a), (select_compressor b))
  | AlgMinus(u, a, b) ->
    AlgMinus(u, (select_compressor a), (select_compressor b))
  | AlgProduct(u, a, b) ->
    AlgProduct(u, (select_compressor a), (select_compressor b))
  | AlgJoin(u, (a, expr_a), (b, expr_b)) ->
    AlgJoin(u, (select_compressor a, expr_a), (select_compressor b, expr_b))
  | AlgRename(u, a, b) ->
    AlgRename(u, select_compressor a, b)
  | AlgProjection(u, a, b) ->
    AlgProjection(u, select_compressor a, b)
  | AlgSelect(u, a, b) ->
    AlgSelect(u, select_compressor a, b)
  | AlgAddColumn(u, a, b, c) ->
    AlgAddColumn(u, select_compressor a, b, c)
  | AlgInput(u, str) ->
    AlgInput(u, str)
  | AlgOrder(u, a, criterion) ->
    AlgOrder(u, select_compressor a, criterion)

(* deduce joins *)
let create_joins alg = 
  let tbl = Hashtbl.create 10 in
  let _ = MetaQuery.get_headers ~f:(fun x y -> Hashtbl.add tbl x y) alg in
  let get_headers query =
    Hashtbl.find tbl (MetaQuery.get_uid_from_alg query)
  in 

  let rec visitor alg = 
    match alg with
    | AlgSelect(u, AlgProduct(u', lhs_query, rhs_query), (AlgBinOp(Ast.Eq, lhs_expr, rhs_expr) as expr)) ->
      let attrs_lhs_expr = attributes_of_condition lhs_expr in
      let attrs_rhs_expr = attributes_of_condition rhs_expr in
      if List.length attrs_lhs_expr = 1 && List.length attrs_rhs_expr = 1 
         && fst @@ List.hd attrs_lhs_expr <> fst @@ List.hd attrs_rhs_expr then 
        let a_lhs = List.hd attrs_lhs_expr in
        let a_rhs = List.hd attrs_rhs_expr in
        let h_lhs = get_headers lhs_query in
        let h_rhs = get_headers rhs_query in
        let in_l e l = Array.exists ((=) e) l in
        if in_l a_lhs h_lhs && in_l a_rhs h_rhs then
          AlgJoin(new_uid(), (visitor lhs_query, lhs_expr), (visitor rhs_query, rhs_expr))
        else if in_l a_lhs h_rhs && in_l a_rhs h_lhs then
          AlgJoin(new_uid(), (visitor lhs_query, rhs_expr), (visitor rhs_query, lhs_expr))
        else 
          AlgSelect(u, AlgProduct(u', visitor lhs_query, visitor rhs_query), expr)
      else 
        AlgSelect(u, AlgProduct(u', visitor lhs_query, visitor rhs_query), expr)

    | AlgSelect(u, a, b) ->
      AlgSelect(u, visitor a, b)
    | AlgProjection(u, a, b) ->
      AlgProjection(u, visitor a, b)
    | AlgRename(u, a, b) ->
      AlgRename(u, visitor a, b)
    | AlgMinus(u, a, b) ->
      AlgMinus(u, visitor a, visitor b)
    | AlgUnion(u, a, b) ->
      AlgUnion(u, visitor a, visitor b)
    | AlgProduct(u, a, b) ->
      AlgProduct(u, visitor a, visitor b)
    | AlgAddColumn(u, a, b, c) ->
      AlgAddColumn(u, visitor a, b, c)
    | AlgJoin(u, (a, ea), (b, eb)) ->
      AlgJoin(u, (visitor a, ea), (visitor b, eb))
    | x -> x
  in visitor alg




(* Projections optimizer *)
      (*
let insert_projections alg = 
  let tbl = Hashtbl.create 10 in
  let _ = MetaQuery.get_headers ~f:(fun x y -> Hashtbl.add tbl x y) alg in
  let get_headers alg =
    Hashtbl.find tbl (MetaQuery.get_uid_from_alg alg)
    |> Array.to_list
    |> SetAttributes.of_list 
  in 
  let rec aux alg =
    let min_header_end = 
      match alg with
      | AlgProjection(u, a, b) ->

  in aux alg
         *)
