type algebra =
  | AlgUnion of algebra * algebra
  | AlgProjection of algebra * string list
  | AlgInput of string (* for input nodes *)

type feed_result = string list

module type Feed = sig
  type t
  type o
  val open_feed : o -> t
  val close_feed : t -> unit
  val reset : t -> unit
  val next : t -> (feed_result option) 
  val headers : t -> string list 
end

module type FeedHandlerInterface = sig
  module FeedHandler : Feed
  val this : FeedHandler.t
end

class virtual feed_interface =
  object
    method virtual next : feed_result option
    method virtual headers : string list
    method virtual reset : unit
  end

