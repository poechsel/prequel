type header = string option * string


type expression =
  | AlgBinOp of Ast.binop * expression * expression
  | AlgAtom of Ast.atom

type algebra =
  | AlgUnion of algebra * algebra
  | AlgProjection of algebra * header list
  | AlgInput of string (* for input nodes *)
  | AlgProduct of algebra * algebra
  | AlgSelect of algebra * expression
  | AlgRenameTable of algebra * string
  | AlgRenameRows of algebra * (string * expression)

type feed_result = string list

class virtual feed_interface =
  object
    method virtual next : feed_result option
    method virtual headers : header list
    method virtual reset : unit
  end
