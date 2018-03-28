open AlgebraTypes
let rec get_subtree_from_uid uid tree = 
  (* get the subtrees having a specified uid *)
  if get_uid_from_alg tree = uid then
    Some tree
  else 
    match tree with
  | AlgUnion(_, a, b) 
  | AlgProduct(_, a, b) 
  | AlgJoin(_, (a, _), (b, _))
  | AlgMinus(_, a, b) ->
    begin match (get_subtree_from_uid uid a) with
      | None -> get_subtree_from_uid uid b
      | Some x -> Some x
    end 
  | AlgAddColumn(_, a, _, _) 
  | AlgOrder(_, a, _) 
  | AlgProjection(_, a, _) 
  | AlgRename(_, a, _)
  | AlgSelect(_, a, _) ->
    get_subtree_from_uid uid a
  | AlgInput _ ->
    None



let rec get_headers ?(f=(fun _ _ -> ())) query =
  let res = match query with
    | AlgInput(_, str) ->
      InputCachedFile.get_headers str
    | AlgUnion(_, a, b) ->
      Union.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgMinus(_, a, b) ->
      Minus.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgJoin(_, (a, _), (b, _)) ->
      Join.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgProjection(_, a, headers) ->
      let _ = get_headers ~f:f a in
      headers
    | AlgSelect(_, a, filter) ->
      Select.get_headers (get_headers ~f:f a)
    | AlgProduct(_, a, b) ->
      Product.get_headers (get_headers ~f:f a) (get_headers ~f:f b)
    | AlgRename(_, a, b) ->
      let h = get_headers ~f:f a in
      let tbl = Rename.build_rename_map b h in
      Rename.get_headers tbl h
    | AlgAddColumn(_, a, _, n) ->
      AddColumn.get_headers (get_headers ~f:f a) n
    | AlgOrder(_, a, criterion) ->
      Select.get_headers (get_headers ~f:f a)
  in 
  let _ = f (get_uid_from_alg query) res in
  res



let feed_from_query ?use_caching:(use_caching=false) ?big_data:(big_data=false) (query : algebra) : feed_interface = 
  (* first create our cache structure *)
  let cache = Caching.create query in
  let rec feed_from_query query =
    (* convert a query to a feed *)
    let aux query = 
      (* auxilliary function to convert to feed a term *)
      match query with
      | AlgInput(_, str)   -> 
        new InputCachedFile.inputCachedFile str
      | AlgUnion(_, a, b) ->
        if not big_data then
          new Union.unionHash (feed_from_query a) (feed_from_query b)
        else
          new Union.unionSort (feed_from_query a) (feed_from_query b)
      | AlgMinus(_, a, b) ->
        if not big_data then
          new Minus.minusHash (feed_from_query a) (feed_from_query b)
        else
          new Minus.minusSort (feed_from_query a) (feed_from_query b)
      | AlgProjection(_, a, headers) ->
        new Projection.projection (feed_from_query a) headers
      | AlgSelect(_, a, filter) ->
        let sub = feed_from_query a in
        let filter = Arithmetics.compile_filter (get_headers a) filter in
        new Select.select sub filter
      | AlgJoin(_, (a, expr_a), (b, expr_b)) ->
        let sub_a = feed_from_query a in
        let eval_a = Arithmetics.compile_value (get_headers a) expr_a in
        let sub_b = feed_from_query b in
        let eval_b = Arithmetics.compile_value (get_headers b) expr_b in
        if not big_data then
            new Join.joinHash (sub_a, eval_a) (sub_b, eval_b)
        else 
          let sub_b = new ExternalSort.sort sub_b [|Arithmetics.compile_value (sub_b#headers) expr_b, Ast.Asc|] in
          new Join.joinSorted (sub_a, eval_a) (sub_b, eval_b)
      | AlgAddColumn(_, a, expr, n) ->
        new AddColumn.addColumn 
          (feed_from_query a) 
          (Arithmetics.compile_value (get_headers a) expr) 
          n
      | AlgProduct(_, a, b) ->
        new Product.product (feed_from_query a) (feed_from_query b)
      | AlgRename(_, a, b) ->
        new Rename.rename (feed_from_query a) (b)
      | AlgOrder(_, a, criterion) ->
        let headers = get_headers a in
        let sub = feed_from_query a in
        let compiled = Array.map
            (fun (v, ord) -> (Arithmetics.compile_value headers v, ord))
            criterion in
        new ExternalSort.sort sub compiled

    in
    (* if the current tree can be cached, build a feed that will get the 
       precomputed results.
       If it is used as the cache, then materialize the results.
       Otherwize build the feed "normally" *)
    if use_caching then
      match (Caching.use_cache cache query) with 
      | Caching.No_cache ->
        aux query
      | Caching.Materialized path ->
        new Materialize.materialize (aux query) path
      | Caching.UnMaterialized path ->
        let headers = get_headers query in
        new Materialize.unmaterialize headers path
    else 
      aux query
  in feed_from_query query
