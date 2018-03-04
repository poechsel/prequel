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

type 'a relation =
  | AstSubQuery of 'a query
  | AstTable of string

and 'a query =
  | AstSelect of attribute_renamed list * 'a relation list * 'a * 'a option
  | AstMinus of 'a query * 'a query
  | AstUnion of 'a query * 'a query

type cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of compop * attribute * attribute
  | AstIn of attribute * cond query
  | AstNotIn of attribute * cond query

type disj = 
  | AstCompOp of compop * attribute * attribute
  | AstIn of attribute * disj list list query
  | AstNotIn of attribute * disj list list query
