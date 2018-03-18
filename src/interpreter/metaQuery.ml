open AlgebraTypes


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
    let filter = Arithmetics.compile_filter sub#headers filter in
    new Select.select sub filter
  | AlgProduct(_, a, b) ->
    new Product.product (feed_from_query a) (feed_from_query b)
  | AlgRenameTable(_, a, b) ->
    new Rename.rename_table (feed_from_query a) (b)

