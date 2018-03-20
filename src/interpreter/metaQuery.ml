open AlgebraTypes

let rec get_headers query =
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

