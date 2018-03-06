type t = { a : AlgebraTypes.feed_handler; b : AlgebraTypes.feed_handler }
type o = AlgebraTypes.feed_handler * AlgebraTypes.feed_handler

let open_feed (a, b) =
  { a = a; b = b }

let close_feed t =
  ()

let headers t = 
  let (module A : AlgebraTypes.FeedHandlerInterface) = t.a in
  let (module B : AlgebraTypes.FeedHandlerInterface) = t.b in
  A.FeedHandler.headers A.this

let next t = 
  let (module A : AlgebraTypes.FeedHandlerInterface) = t.a in
  let (module B : AlgebraTypes.FeedHandlerInterface) = t.b in
  match A.FeedHandler.next A.this with
  | None -> 
    B.FeedHandler.next B.this
  | Some x -> Some x

let reset t = 
  let (module A : AlgebraTypes.FeedHandlerInterface) = t.a in
  let (module B : AlgebraTypes.FeedHandlerInterface) = t.b in
  let _ = A.FeedHandler.reset A.this in
  let _ = B.FeedHandler.reset B.this in
  ()
