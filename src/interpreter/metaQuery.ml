open AlgebraTypes

let get_uid_from_alg a = 
  match a with
  | AlgInput(u, _) 
  | AlgUnion(u, _, _)
  | AlgMinus(u, _, _)
  | AlgProjection(u, _, _)
  | AlgProduct(u, _, _)
  | AlgJoin(u, _, _)
  | AlgSelect(u, _, _)
  | AlgRename(u, _, _) ->
    u



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
  in 
  let _ = f (get_uid_from_alg query) res in
  res





let rec feed_from_query (query : algebra) : feed_interface = 
  (* convert a query to a feed *)
  match query with
  | AlgInput(_, str)   -> 
    new InputCachedFile.inputCachedFile str
  | AlgUnion(_, a, b) ->
    new Union.union (feed_from_query a) (feed_from_query b)
  | AlgMinus(_, a, b) ->
    new Minus.minus (feed_from_query a) (feed_from_query b)
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
    (* WE MUST SORT the right hand side *)
    let sub_b = new ExternalSort.sort sub_b [|expr_b|] in
    new Join.joinSorted (sub_a, eval_a) (sub_b, eval_b)
      (*
      (* fastest for small tables *)
    new Join.joinHash (sub_a, eval_a) (sub_b, eval_b)
         *)
  | AlgProduct(_, a, b) ->
    new Product.product (feed_from_query a) (feed_from_query b)
  | AlgRename(_, a, b) ->
    new Rename.rename (feed_from_query a) (b)

