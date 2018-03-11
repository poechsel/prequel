open AlgebraTypes


let rec feed_from_query (query : algebra) : feed_interface = 
  (* convert a query to a feed *)
  match query with
  | AlgInput(str)   -> 
    new InputCachedFile.inputCachedFile str
  | AlgUnion(a, b) ->
    new Union.union (feed_from_query a) (feed_from_query b)
  | AlgMinus(a, b) ->
    new Minus.minus (feed_from_query a) (feed_from_query b)
  | AlgProjection(a, headers) ->
    new Projection.projection (feed_from_query a) headers
  | AlgSelect(a, filter) ->
    new Select.select (feed_from_query a) filter
  | AlgProduct(a, b) ->
    new Product.product (feed_from_query a) (feed_from_query b)
  | AlgRenameTable(a, b) ->
    new Rename.rename_table (feed_from_query a) (b)
