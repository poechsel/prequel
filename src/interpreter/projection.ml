type t = { keep : int list; sub : AlgebraTypes.feed_handler }
type o = AlgebraTypes.feed_handler * string list

let open_feed (sub, headers) =
  let (module Sub : AlgebraTypes.FeedHandlerInterface) = sub in
  let all_headers = Sub.FeedHandler.headers Sub.this in
  let keep = List.filter (fun x -> x >= 0) 
    (List.mapi (fun i x ->
        if List.exists (fun a -> a = x) headers then i
        else -1
       ) all_headers)
  in { keep = keep; sub = sub }


let close_feed t = 
  ()


let next c = 
  let (module Sub : AlgebraTypes.FeedHandlerInterface) = c.sub in
  let next = Sub.FeedHandler.next Sub.this in
  match next with
  | None -> None
  | Some x ->
      Some (List.map (fun i -> List.nth x i) c.keep)

let reset c = 
  let (module Sub : AlgebraTypes.FeedHandlerInterface) = c.sub in
  Sub.FeedHandler.reset Sub.this

let headers c = 
  let (module Sub) = c.sub in
  List.map (fun i -> List.nth (Sub.FeedHandler.headers Sub.this) i) c.keep
