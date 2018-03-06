%{
open Ast
%}

%token <string> ID
%token SELECT WHERE GROUP BY FROM ORDER UNION MINUS
%token AND OR NOT IN LT GT LEQ GEQ EQ NEQ PUNKT COMA 
%token LPAR RPAR AS ENDLINE

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
    | attribute EQ attribute
        { AstCompOp(Eq, $1, $3) }
    | attribute LT attribute
        { AstCompOp(Lt, $1, $3) }
    | attribute IN LPAR query RPAR
        { AstIn($1, $4) }
    | attribute NOT IN LPAR query RPAR
        { AstIn($1, $5) }
        
