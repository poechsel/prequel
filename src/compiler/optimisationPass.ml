open AlgebraTypes

(* computed the 'xor' of two lists *)
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


(* modules used to represent sets of attributes 
   (unordered headers in other words).
   Allows easier set operations *)
module SetAttributes = Set.Make (struct
    type t = AlgebraTypes.header
    let compare = Pervasives.compare
  end )




(* push down select optimizations *)
let push_down_select alg = 
  (* first compute all headers of the tree
     and store them in a hashtbl for faster retrieval *)
  let tbl = Hashtbl.create 10 in
  let _ = MetaQuery.get_headers ~f:(fun x y -> Hashtbl.add tbl x y) alg in
  let get_headers query =
    Hashtbl.find tbl (MetaQuery.get_uid_from_alg query)
    |> Array.to_list
    |> SetAttributes.of_list 
  in 

  (* the main algorithm 
     can_be_pushed: expressions of the selections that we want to
        try and push down in the tree
     alg : the tree
  *)
  let rec push_down can_be_pushed alg =
    let analyze_sub alg =
      (* when analyzing a tree rooted by alg we can splits the select 
         in two parts:
          - the ones we can push down further (`push`)
         - the ones we can can't push down anymore (`stay`)
      *)
      let header = get_headers alg in
      let push, stay =
        can_be_pushed 
        |> List.partition (fun x -> SetAttributes.subset (snd x) header) 
      in stay, push_down push alg
    in 
    (* we get the selectors we can't push down any further
       and the transformed tree *)
    let to_insert, req = 
      match alg with
      | AlgUnion(u, a, b) ->
        (* if x is a selector and can be pushed in
           a or in b, then we have no need to keep it *)
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
        (* add the selector to the list of selectors,
          and returned the transformed tree*)
        let attrs = attributes_of_condition filter |> SetAttributes.of_list in
        let a' = push_down ((filter, attrs) :: can_be_pushed) a in
        [], a'

      | AlgInput(u, str) ->
        can_be_pushed, alg

      | AlgOrder(u, a, criterion) ->
        let i, a' = analyze_sub a in
        i, AlgOrder(u, a', criterion)

    (* we insert the selectors that can't be pushed down *)
    in List.fold_left (fun a (cond, _) -> AlgSelect(new_uid (), a, cond))
         req
         to_insert
  in push_down [] alg




(* SELECT compressor 
   We compress select(select(..., s1), s2) to select(..., s1 AND s2)
   *)
let rec select_compressor alg =
  match alg with 
  | AlgSelect(_, AlgSelect(_, sub, e1), e2) ->
    (* beware of how we merge two selectors. We want
       to create something of the form (a AND (b AND (c AND d))) so
       that lazy evaluation is more efficient *)
    select_compressor (AlgSelect(new_uid(), sub, AlgBinOp(Ast.And, e2, e1)))
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


(* deduce joins operations.
   We convert patterns of the form
   SELECT(PRODUCT(a, b), a.foo==b.bar)
   into a join.

   Why do it both in compilation and in optimisations ?
   Because this pattern can appear after other optimisations steps.
   Patterns like this typically appear when compiling IN.

   We still emit some joins during compilations as it
   allow for more joins to be created
*)
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
      (* check if it is indeed a join *)
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
    | AlgOrder(u, a, criterion) ->
      AlgOrder(u, visitor a, criterion)
    | AlgInput _ as alg ->
      alg
  in visitor alg




(* Projections optimizer.
    We try to keep the least amount of rows possible
*)

(* delete all projections from a query *)
let rec delete_projections alg = 
  match alg with
  | AlgProjection(u, a, b) ->
    a
  | AlgSelect(u, a, b) ->
    AlgSelect(u, delete_projections a, b)
  | AlgRename(u, a, b) ->
    AlgRename(u, delete_projections a, b)
  | AlgMinus(u, a, b) ->
    AlgMinus(u, delete_projections a, delete_projections b)
  | AlgUnion(u, a, b) ->
    AlgUnion(u, delete_projections a, delete_projections b)
  | AlgProduct(u, a, b) ->
    AlgProduct(u, delete_projections a, delete_projections b)
  | AlgJoin(u, (a, ea), (b, eb)) ->
    AlgJoin(u, (delete_projections a, ea), (delete_projections b, eb))
  | AlgInput _ as alg -> 
    alg
  | AlgAddColumn(u, a, b, c) ->
    AlgAddColumn(u, delete_projections a, b, c)
  | AlgOrder(u, a, criterion) ->
    AlgOrder(u, delete_projections a, criterion)


(* add projections back *)
let optimize_projections alg = 
  (* a method to insert projections for a tree `sub` if needed.
     `headers` is the minimum set of headers that `sub` must return
     `headers_before` is the set of headers `sub` must take as input.
     We add a projection if `sub` needs more headers to be executed
     than headers needed after its executions
     *)
  let insert_projection headers headers_before sub =
      if SetAttributes.cardinal headers <> SetAttributes.cardinal headers_before then
        AlgProjection(new_uid ()
                     , sub
                     , SetAttributes.elements headers |> Array.of_list)
      else 
        sub
  in 
  (* a structure for faster lookup of headers *)
  let tbl = Hashtbl.create 10 in
  let get_headers query =
    Hashtbl.find tbl (MetaQuery.get_uid_from_alg query)
    |> Array.to_list
    |> SetAttributes.of_list 
  in 

  let rec visitor headers alg = 
    (* at this point, we want to be sure that headers is something
       included all of the headers accessible by the query represented
       by alg: we do a set intersection operation
       (we are sure that headers is included in alg_headers *)
    let headers = 
      get_headers alg 
      |> SetAttributes.inter headers
    in 
    (* alias to lighten notations *)
    let v = visitor headers in
    match alg with
    | AlgProjection(u, a, b) ->
      AlgProjection(u, v a, b)
    | AlgMinus(u, a, b) ->
      AlgMinus(u, v a, v b)
    | AlgProduct(u, a, b) ->
      AlgProduct(u, v a, v b)
    | AlgUnion(u, a, b) ->
      AlgUnion(u, v a, v b)
    | AlgRename(u, a, l) ->
      let headers = 
        headers
        |> SetAttributes.elements
        |> Rename.inverse_rename l
        |> SetAttributes.of_list
      in AlgRename(u, visitor headers a, l)
    | AlgAddColumn(u, a, b, name) ->
      let headers_before = 
        (*we do not delete ("", name) here because we would have
          added it back *)
        attributes_of_condition b
        |> SetAttributes.of_list 
        |> SetAttributes.union headers
      in
      (* now we delete it*)
      let sub = AlgAddColumn(u, 
                             visitor (SetAttributes.remove ("", name) headers_before) a, 
                             b, 
                             name) in
      insert_projection 
        headers 
        headers_before
        sub
    | AlgSelect(u, a, expr) ->
      (* take into account that `expr` might
         need headers not needed before *)
      let headers_before =
        attributes_of_condition expr
        |> SetAttributes.of_list
        |> SetAttributes.union headers
      in
      let sub = AlgSelect(u, 
                          visitor headers_before a, 
                          expr) in 
      insert_projection 
        headers 
        headers_before
        sub

    | AlgJoin(u, (a, ea), (b, eb)) ->
      let headers_ea = 
        attributes_of_condition ea
        |> SetAttributes.of_list 
      in let headers_eb = 
        attributes_of_condition eb
        |> SetAttributes.of_list 
      in let headers_before =
        SetAttributes.union headers_ea headers_eb
        |> SetAttributes.union headers
      in
      let sub = AlgJoin(u, 
                        (visitor headers_before a, ea), 
                        (visitor headers_before b, eb)) in 
      insert_projection 
        headers 
        headers_before
        sub

    | AlgInput(u, path) ->
      let headers_before = 
        InputCachedFile.get_headers path
        |> Array.to_list
        |> SetAttributes.of_list 
        |> SetAttributes.union headers
      in
      let sub = AlgInput(u, path)
      in insert_projection
        headers
        headers_before
        sub

    | AlgOrder(u, a, exprs_array) ->
      let exprs = 
        exprs_array
        |> Array.to_list
        |> List.map fst
        |> List.map attributes_of_condition
        |> List.map SetAttributes.of_list 
      in let headers_before = 
           exprs
           |> Utils.merge_list (SetAttributes.union) 
           |> SetAttributes.union headers
      in let sub = AlgOrder(u, 
                   (visitor headers_before a),
                   exprs_array)
      in insert_projection
        headers
        headers_before
        sub

  in
  (* we get the headers we want at the end
     of the execution. It will serve as the 
     root of our traversal *)
  let headers = MetaQuery.get_headers alg

  (* we use sets in order to speed up computations *)
  in let headers_set = 
    headers
    |> Array.to_list
    |> SetAttributes.of_list 
  in
  (* we remove all projections *)
  let alg = delete_projections alg in
  (* we compute the headers that are seen on
     each constructor *)
  let _ = MetaQuery.get_headers ~f:(fun x y -> Hashtbl.add tbl x y) alg in
  (* finally we insert the projections when needed *)
  let alg = visitor headers_set alg in
  (* unfortunately we've probably lost the order of the last 
     projection here. To solve this problem we had a last projection
     which reorder the rows *)
  AlgProjection(new_uid (), alg, headers)
