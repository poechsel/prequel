type attribute = string option * string
type attribute_renamed = attribute * string option


type binop = 
  | And
  | Or
  | Add
  | Sub
  | Times
  | Div

type compop =
  | Leq
  | Eq
  | Neq
  | Geq
  | Lt
  | Gt

type exprop =
  | Add
  | Sub
  | Div
  | Times

type expression =
  | AstExprOp of exprop * expression * expression
  | AstAttribute of attribute
  | AstNumber of int
  | AstString of string

type 'a relation =
  | AstSubQuery of 'a query
  | AstTable of string

and 'a query =
  | AstSelect of attribute_renamed list * ('a relation * string option) list * 'a option
  | AstMinus of 'a query * 'a query
  | AstUnion of 'a query * 'a query

type cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of compop * expression * expression
  | AstIn of expression * cond query
  | AstNotIn of expression * cond query


type disj = 
  | DisjCompOp of compop * attribute * attribute
  | DisjIn of attribute * (disj list list) query
  | DisjNotIn of attribute * (disj list list) query

type disj_no_or = 
  | DisjNOCompOp of compop * attribute * attribute
  | DisjNOIn of attribute * (disj_no_or list) query
  | DisjNONotIn of attribute * (disj_no_or list) query

