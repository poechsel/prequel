type attribute = string * string
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

type ('a, 'b) relation =
  | AstSubQuery of ('a, 'b) query
  | AstTable of string
  | AstCompiled of 'b

and ('a, 'b) query =
  | AstSelect of attribute_renamed list * (('a, 'b) relation * string) list * 'a option
  | AstMinus of ('a, 'b) query * ('a, 'b) query
  | AstUnion of ('a, 'b) query * ('a, 'b) query

type 'b cond =
  | AstBinOp of binop * 'b cond * 'b cond
  | AstCompOp of binop * expression * expression
  | AstIn of expression * ('b cond, 'b) query
  | AstNotIn of expression * ('b cond, 'b) query


(* 
   Expressions are represented with a disjunctive form
*)
type 'b disj = 
  | DisjCompOp of binop * expression * expression
  | DisjIn of expression * ('b disj list list, 'b) query
  | DisjNotIn of expression * ('b disj list list, 'b) query
