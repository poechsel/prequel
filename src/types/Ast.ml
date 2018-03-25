type attribute = string * string
[@@deriving show]
type attribute_renamed = attribute * string option
[@@deriving show]

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
[@@deriving show]

type atom =
  | Attribute of attribute
  | Number of int
  | String of string
[@@deriving show]

type expression =
  | AstExprOp of binop * expression * expression
  | AstAtom of atom
[@@deriving show]

type attribute_select =
  | AstSeRenamed    of attribute_select * string
  | AstSeExpr       of expression
  | AstSeAttribute  of attribute        
[@@deriving show]


type ('a, 'b) relation =
  | AstSubQuery of ('a, 'b) query
  | AstTable of string
  | AstCompiled of 'b
[@@deriving show]

and ('a, 'b) query =
  | AstSelect of attribute_select list * (('a, 'b) relation * string) list * 'a option
  | AstMinus of ('a, 'b) query * ('a, 'b) query
  | AstUnion of ('a, 'b) query * ('a, 'b) query
[@@deriving show]

type 'b cond =
  | AstBinOp of binop * 'b cond * 'b cond
  | AstCompOp of binop * expression * expression
  | AstIn of expression * ('b cond, 'b) query
  | AstNotIn of expression * ('b cond, 'b) query
[@@deriving show]


(* 
   Expressions are represented with a disjunctive form
*)
type 'b disj = 
  | DisjCompOp of binop * expression * expression
  | DisjIn of expression * ('b disj list list, 'b) query
  | DisjNotIn of expression * ('b disj list list, 'b) query
[@@deriving show]
