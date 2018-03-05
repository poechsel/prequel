type algebra =
  | AlgUnion of algebra * algebra
  | AlgInput of string (* for input nodes *)

type feed_result = string

module type Feed = sig
  type t
  type o
  val open_feed : o -> t
  val close_feed : t -> unit
  val reset : t -> unit
  val next : t -> (feed_result option) 
end

module type FeedHandlerInterface = sig
  module FeedHandler : Feed
  val this : FeedHandler.t
end
  
  
type feed_handler = (module FeedHandlerInterface)
