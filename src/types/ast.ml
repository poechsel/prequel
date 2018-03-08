type attribute = string option * string
type attribute_renamed = attribute * string option


type binop = 
  | And
  | Or
  | Leq
  | Eq
  | Neq
  | Geq
  | Lt
  | Gt
  | Add
  | Sub
  | Div
  | Times

type atom =
  | Attribute of attribute
  | Number of int
  | String of string

type expression =
  | AstExprOp of binop * expression * expression
  | AstAtom of atom

type 'a relation =
  | AstSubQuery of 'a query
  | AstTable of string

and 'a query =
  | AstSelect of attribute_renamed list * ('a relation * string) list * 'a option
  | AstMinus of 'a query * 'a query
  | AstUnion of 'a query * 'a query

type cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of binop * expression * expression
  | AstIn of expression * cond query
  | AstNotIn of expression * cond query


type disj = 
  | DisjCompOp of binop * expression * expression
  | DisjIn of expression * (disj list list) query
  | DisjNotIn of expression * (disj list list) query

type disj_no_or = 
  | DisjNOCompOp of binop * attribute * attribute
  | DisjNOIn of attribute * (disj_no_or list) query
  | DisjNONotIn of attribute * (disj_no_or list) query

