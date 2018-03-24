%{
    open Command
    open Ast
%}

%token <string> ID
%token <string> STRING
%token <int> NUMBER
%token SELECT WHERE GROUP BY FROM ORDER UNION MINUS
%token AND OR NOT IN LT GT LEQ GEQ EQ NEQ PUNKT COMA 
%token LPAR RPAR AS ENDLINE TIMES ADD SUB DIV EOF

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


/* Attributes */
attributes:
    | TIMES           { [] }
    | attributes_list { $1 }

attributes_list:
    | attribute_renamed                      { [$1] }
    | attribute_renamed COMA attributes_list { $1 :: $3 }

attribute_renamed:
    | attribute AS ID { $1, Some $3 }
    | attribute       { $1, None }

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

relation_list:
    | relation                    { [$1] }
    | relation COMA relation_list { $1 :: $3 }


/* Conditions */
condition:
    | and_condition OR condition            { AstBinOp (Or, $1, $3) }
    | and_condition                         { $1 }

and_condition:
    | at_condition AND and_condition        { AstBinOp(And, $1, $3) }
    | at_condition                          { $1 }

at_condition:
    | LPAR condition RPAR                   { $2 }
    | add_expression EQ add_expression      { AstCompOp(Eq, $1, $3) }
    | add_expression LT add_expression      { AstCompOp(Lt, $1, $3) }
    | add_expression IN LPAR query RPAR     { AstIn($1, $4) }
    | add_expression NOT IN LPAR query RPAR { AstNotIn($1, $5) }
        

/* Expressions */
add_expression:
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
query:
    | SELECT attributes FROM relation_list WHERE condition
        { AstSelect($2, $4, Some $6) }
    | SELECT attributes FROM relation_list
        { AstSelect($2, $4, None) }
    | LPAR query RPAR MINUS LPAR query RPAR
        { AstMinus($2, $6) }
    | LPAR query RPAR UNION LPAR query RPAR
        { AstUnion($2, $6) }