type attribute = string

type binop = 
  | And
  | Or

type compop =
  | LEQ
  | EQ
  | NEQ
  | GEQ
  | LT
  | GT

type relation =
  | AstSubQuery of ast
  | AstTable of string

and cond =
  | AstBinOp of binop * cond * cond
  | AstCompOp of compop * attribute * attribute
  | AstIn of attribute * ast
  | AstNotIn of attribute * ast

and ast =
  | AstSelect of attribute list * relation list * cond * ast option
  | AstMinus of ast * ast
  | AstUnion of ast * ast
