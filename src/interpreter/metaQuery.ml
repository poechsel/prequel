open AlgebraTypes

let get_uid_from_alg a = 
  match a with
  | AlgInput(u, _) 
  | AlgUnion(u, _, _)
  | AlgMinus(u, _, _)
  | AlgProjection(u, _, _)
  | AlgProduct(u, _, _)
  | AlgSelect(u, _, _)
  | AlgRenameTable(u, _, _) ->
    u



let rec get_headers ?(f=(fun _ _ -> ())) query =
  let aux query = 
    let get_headers = get_headers ~f:f in
    match query with
    | AlgInput(_, str) ->
      InputCachedFile.get_headers str
    | AlgUnion(_, a, b) ->
      Union.get_headers (get_headers a) (get_headers b)
    | AlgMinus(_, a, b) ->
      Minus.get_headers (get_headers a) (get_headers b)
    | AlgProjection(_, a, headers) ->
      headers
    | AlgSelect(_, a, filter) ->
      Select.get_headers (get_headers a)
    | AlgProduct(_, a, b) ->
      Product.get_headers (get_headers a) (get_headers b)
    | AlgRenameTable(_, a, b) ->
      Rename.get_headers b (get_headers a)
  in 
  let res = aux query in
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
  | AlgProduct(_, a, b) ->
    new Product.product (feed_from_query a) (feed_from_query b)
  | AlgRenameTable(_, a, b) ->
    new Rename.rename_table (feed_from_query a) (b)

