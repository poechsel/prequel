%{
    open Command
    open Ast
%}

%token <string> ID
%token <string> STRING
%token <int> NUMBER
%token SELECT WHERE HAVING GROUP BY FROM ORDER UNION MINUS
%token AND OR NOT IN LT GT LEQ GEQ EQ NEQ PUNKT COMA ASC DESC
%token LPAR RPAR AS ENDLINE TIMES ADD SUB DIV EOF

%left MINUS ADD DIV MULT
%nonassoc UMINUS 

%start main
%type<Command.t> main
%%

main:
  /* Top-level commands */
  | PUNKT ID ENDLINE        { Command ($2, None) }
  | PUNKT ID ID ENDLINE     { Command ($2, Some $3) }
  | PUNKT ID STRING ENDLINE { Command ($2, Some $3) }

  /* SQL queries */
  | query ENDLINE { Query ($1) }
  | query EOF     { Query ($1) }


attribute_select_ren:
    | attribute_select       { $1 }
    | attribute_select ID { AstSeRenamed ($1, $2) }
    | attribute_select AS ID { AstSeRenamed ($1, $3) }

attribute_select:
    | add_expression        { match $1 with
                                | AstAtom(Attribute x) -> AstSeAttribute x
                                | x -> AstSeExpr x}

attribute:
    | ID PUNKT ID     { $1, $3 }
    | ID              { "", $1 }


/* Relations */
relation:
    | relation_atom AS ID         { $1, $3 }
    | relation_atom ID            { $1, $2 }

relation_atom:
    | STRING                      { AstTable $1 }
    | LPAR query RPAR             { AstSubQuery $2 }


/* Conditions */
condition:
    | and_condition OR condition            { AstBinOp (Or, $1, $3) }
    | and_condition                         { $1 }

and_condition:
    | at_condition AND and_condition        { AstBinOp(And, $1, $3) }
    | at_condition                          { $1 }

at_condition:
    | LPAR condition RPAR                   { $2 }
    | add_expression comp add_expression    { AstCompOp($2, $1, $3) }
    | add_expression IN LPAR query RPAR     { AstIn($1, $4) }
    | add_expression NOT IN LPAR query RPAR { AstNotIn($1, $5) }
        

/* Comparison operators */
comp:
  | LT  { Lt }
  | GT  { Gt }
  | LEQ { Leq }
  | GEQ { Geq }
  | EQ  { Eq }
  | NEQ { Neq }


/* Expressions */
add_expression:
    | SUB add_expression %prec UMINUS       { AstExprOp(Sub, AstAtom (Number 0), $2) }
    | mult_expression ADD add_expression    { AstExprOp(Add, $1, $3) }
    | mult_expression SUB add_expression    { AstExprOp(Sub, $1, $3) }
    | mult_expression                       { $1 }

mult_expression:
    | atom TIMES mult_expression            { AstExprOp(Times, AstAtom $1, $3) }
    | atom DIV mult_expression              { AstExprOp(Div, AstAtom $1, $3) }
    | LPAR add_expression RPAR              { $2 }
    | atom                                  { AstAtom $1 }


/* Atoms */
atom:
    | NUMBER    { Number $1 }
    | STRING    { String $1 }
    | attribute { Attribute $1 }


/* Queries */
select:
  | SELECT TIMES { [] }
  | SELECT separated_list(COMA, attribute_select_ren) { $2 } 
from:
  | FROM separated_list(COMA, relation) { $2 }
where:
  | WHERE condition { $2 }
order_criteria:
  | add_expression      { ($1, Asc) }
  | add_expression ASC  { ($1, Asc) }
  | add_expression DESC { ($1, Desc) }
order:
  | ORDER BY separated_list(COMA, order_criteria) { $3 }
group:
  | GROUP BY separated_list(COMA, add_expression) { $3 }
having:
  | HAVING condition { $2 }

query:
    | s = select
      f = from
      w = option(where)
      o = option(order)
      g = option(group)
      h = option(having)
        { AstSelect(s, f, w, o, g, h) }
    | LPAR query RPAR MINUS LPAR query RPAR
        { AstMinus($2, $6) }
    | LPAR query RPAR UNION LPAR query RPAR
        { AstUnion($2, $6) }
