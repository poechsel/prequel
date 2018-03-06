%{
open Ast
%}

%token <string> ID
%token <string> STRING
%token <int> NUMBER
%token SELECT WHERE GROUP BY FROM ORDER UNION MINUS
%token AND OR NOT IN LT GT LEQ GEQ EQ NEQ PUNKT COMA 
%token LPAR RPAR AS ENDLINE TIMES ADD SUB DIV

%start main
%type<Ast.cond Ast.query> main
%%

main:
    | query ENDLINE
        { $1 }

attributes_list:
    | attribute_renamed
        { [$1] }
    | attribute_renamed COMA attributes_list
        { $1 :: $3}

attribute_renamed:
    | attribute AS ID
        { $1, Some $3 }
    | attribute
        { $1, None }

attribute:
    | ID PUNKT ID
        { Some $1, $3 }
    | ID 
        { None, $1}


relation:
    | relation_atom AS ID
        { $1, Some $3 }
    | relation_atom
        { $1, None }

relation_atom:
    | ID 
        { AstTable $1 }
    | LPAR query RPAR
        { AstSubQuery $2 }

relation_list:
    | relation
        { [$1] }
    | relation "," relation_list
        { $1 :: $3 }

query:
    | SELECT TIMES FROM relation_list WHERE cond 
        { AstSelect([], $4, Some $6) }
    | SELECT TIMES FROM relation_list 
        { AstSelect([], $4, None) }
    | SELECT attributes_list FROM relation_list WHERE cond 
        { AstSelect($2, $4, Some $6) }
    | SELECT attributes_list FROM relation_list 
        { AstSelect($2, $4, None) }
    | LPAR query RPAR MINUS LPAR query RPAR
        { AstMinus($2, $6) }
    | LPAR query RPAR UNION LPAR query RPAR
        { AstUnion($2, $6) }

cond:
    | and_cond OR cond 
        { AstBinOp (Or, $1, $3) }
    | and_cond
        { $1 }

and_cond:
    | at_cond AND and_cond 
        { AstBinOp(And, $1, $3) }
    | at_cond
        { $1 }

at_cond:
    | LPAR cond RPAR
        { $2 }
    | add_expression EQ add_expression
        { AstCompOp(Eq, $1, $3) }
    | add_expression LT add_expression
        { AstCompOp(Lt, $1, $3) }
    | add_expression IN LPAR query RPAR
        { AstIn($1, $4) }
    | add_expression NOT IN LPAR query RPAR
        { AstIn($1, $5) }
        

add_expression:
    | mult_expression ADD add_expression
        { AstExprOp(Add, $1, $3) }
    | mult_expression SUB add_expression
        { AstExprOp(Sub, $1, $3) }
    | mult_expression 
        { $1 }

mult_expression:
    | atom TIMES mult_expression
        { AstExprOp(Times, AstAtom $1, $3) }
    | atom DIV mult_expression
        { AstExprOp(Div, AstAtom $1, $3) }
    | LPAR add_expression RPAR
        { $2 }
    | atom 
        { AstAtom $1 }

atom:
    | NUMBER
        { Number $1 }
    | STRING
        { String $1 }
    | attribute
        { Attribute $1 }
