%{
    open Command
    open Ast
%}

%token <string> ID
%token <string> STRING
%token <int> NUMBER
%token SELECT WHERE HAVING GROUP BY FROM ORDER UNION MINUS
%token AND OR NOT IN LT GT LEQ GEQ EQ NEQ PUNKT COMA ASC DESC
%token LPAR RPAR AS ENDLINE TIMES ADD SUB DIV MIN MAX AVG COUNT EOF
%token NOOP

%nonassoc UMINUS 

%start main
%type<Command.t> main
%%

main:
  /* Top-level commands */
  | PUNKT ID ENDLINE         { Command ($2, None) }
  | PUNKT ID ID ENDLINE      { Command ($2, Some $3) }
  | PUNKT ID STRING ENDLINE  { Command ($2, Some $3) }

  /* SQL queries */
  | query ENDLINE  { Query ($1) }
  | query EOF      { Query ($1) }


/* Attributes */
attribute_select_ren:
  | attribute_select        { $1 }
  | attribute_select ID     { AstSeRenamed ($1, $2) }
  | attribute_select AS ID  { AstSeRenamed ($1, $3) }

attribute_select:
  | agg_expression {
      match $1 with
        | AstAtom(Attribute x) -> AstSeAttribute x
        | x -> AstSeExpr x }

attribute:
  | ID PUNKT ID  { $1, $3 }
  | ID           { "", $1 }


/* Relations */
relation:
  | relation_atom AS ID  { $1, $3 }
  | relation_atom ID     { $1, $2 }

relation_atom:
  | STRING               { AstTable $1 }
  | LPAR query RPAR      { AstSubQuery $2 }


/* Expressions */
%inline no_agg:
  | NOOP   { failwith "Shouldn't happen." }

/* An expression which doesn't contain aggregate functions. */
std_expression: add_expression(no_agg) { $1 }

%inline all_agg:
  | MIN    { Min }
  | MAX    { Max }
  | AVG    { Avg }
  | COUNT  { Count }

/* An expression which might contain aggregate functions. */
agg_expression: add_expression(all_agg) { $1 }

add_expression(agg):
  | SUB add_expression(agg) %prec UMINUS          { AstExprOp(Sub, AstAtom (Number 0), $2) }
  | mult_expression(agg) ADD add_expression(agg)  { AstExprOp(Add, $1, $3) }
  | mult_expression(agg) SUB add_expression(agg)  { AstExprOp(Sub, $1, $3) }
  | mult_expression(agg)                          { $1 }

mult_expression(agg):
  | atom TIMES mult_expression(agg) { AstExprOp(Times, AstAtom $1, $3) }
  | atom DIV mult_expression(agg)   { AstExprOp(Div, AstAtom $1, $3) }
  | LPAR add_expression(agg) RPAR   { $2 }
  | atom                            { AstAtom $1 }

  /* We only allow aggregate function calls on attributes for the moment */
  | agg LPAR attribute RPAR         { AstExprAgg($1, $3) }


/* Conditions */
std_condition: condition(std_expression) { $1 }
agg_condition: condition(agg_expression) { $1 }

condition(expr):
  | and_condition(expr) OR condition(expr)  { AstBinOp (Or, $1, $3) }
  | and_condition(expr)                     { $1 }

and_condition(expr):
  | at_condition(expr) AND and_condition(expr)  { AstBinOp(And, $1, $3) }
  | at_condition(expr)                          { $1 }

at_condition(expr):
  | LPAR condition(expr) RPAR    { $2 }
  | expr comp expr               { AstCompOp($2, $1, $3) }
  | expr IN LPAR query RPAR      { AstIn($1, $4) }
  | expr NOT IN LPAR query RPAR  { AstNotIn($1, $5) }
        

/* Comparison operators */
comp:
  | LT   { Lt }
  | GT   { Gt }
  | LEQ  { Leq }
  | GEQ  { Geq }
  | EQ   { Eq }
  | NEQ  { Neq }


/* Atoms */
atom:
  | NUMBER     { Number $1 }
  | STRING     { String $1 }
  | attribute  { Attribute $1 }


/* Queries */
select:
  | SELECT TIMES { [] }
  | SELECT separated_list(COMA, attribute_select_ren) { $2 } 
from:
  | FROM separated_list(COMA, relation) { $2 }
where:
  | WHERE std_condition { $2 }
order_criteria:
  | std_expression       { ($1, Asc) }
  | std_expression ASC   { ($1, Asc) }
  | std_expression DESC  { ($1, Desc) }
order:
  | ORDER BY separated_list(COMA, order_criteria) { $3 }
group:
  | GROUP BY separated_list(COMA, std_expression) { $3 }
having:
  | HAVING agg_condition { $2 }

query:
  | s=select f=from w=where? o=order? g=group? h=having?
    { AstSelect(s, f, w, o, g, h, []) }
  | LPAR query RPAR MINUS LPAR query RPAR
    { AstMinus($2, $6) }
  | LPAR query RPAR UNION LPAR query RPAR
    { AstUnion($2, $6) }
