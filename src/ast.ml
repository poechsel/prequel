type attribute = string option * string
type attribute_renamed = attribute * string option


type binop = 
  | And
  | Or

type compop =
  | Leq
  | Eq
  | Neq
  | Geq
  | Lt
  | Gt

type relation =
  | AstSubQuery of query
  | AstTable of string

and cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of compop * attribute * attribute
  | AstIn of attribute * query
  | AstNotIn of attribute * query

and query =
  | AstSelect of attribute_renamed list * relation list * cond * cond option
  | AstMinus of query * query
  | AstUnion of query * query
