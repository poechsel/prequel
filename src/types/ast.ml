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
  | AstSelect of attribute_renamed list * ('a relation * string option) list * 'a 
  | AstMinus of 'a query * 'a query
  | AstUnion of 'a query * 'a query

type cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of compop * attribute * attribute
  | AstIn of attribute * cond query
  | AstNotIn of attribute * cond query

type disj = 
  | DisjCompOp of compop * attribute * attribute
  | DisjIn of attribute * (disj list list) query
  | DisjNotIn of attribute * (disj list list) query

type disj_no_or = 
  | DisjNOCompOp of compop * attribute * attribute
  | DisjNOIn of attribute * (disj_no_or list) query
  | DisjNONotIn of attribute * (disj_no_or list) query

