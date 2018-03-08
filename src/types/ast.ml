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


(* Here a query seector is represented in a list of tuples of list :
   Elements of a list are connected through a OR op.

   The left element of a tuple only contains expression without in and not in.
   The right elements contains expressions with an in or a not in (at least one).

   Expressions are represented as a list of sub-expressions connected by a AND.
   This is nearly a disjunctive form.
*)
type disj = 
  | DisjCompOp of binop * expression * expression
  | DisjIn of expression * (disj list list * disj list list) query
  | DisjNotIn of expression * (disj list list * disj list list) query
