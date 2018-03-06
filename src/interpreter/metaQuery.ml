open AlgebraTypes



let rec feed_from_query (query : algebra) : feed_handler = 
  (* convert a query to a feed *)
  match query with
  | AlgUnion(a, b)  -> 
    let module_a = feed_from_query a in
    let module_b = feed_from_query b in
    (module struct
        module FeedHandler = Union
        let this = Union.open_feed (module_a, module_b)
      end : FeedHandlerInterface
    )
  | AlgProjection(a, keep) ->
    let module_a = feed_from_query a in
    (module struct
        module FeedHandler = Projection
        let this = Projection.open_feed (module_a, keep)
      end : FeedHandlerInterface
    )
  | AlgInput(str)   -> 
    (module struct 
      module FeedHandler = InputCachedFile
      let this = InputCachedFile.open_feed str
    end : FeedHandlerInterface
    )
