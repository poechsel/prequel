{
    open Parser
    exception Eof

    let incr_linenum lexbuf =
        let pos = lexbuf.Lexing.lex_curr_p in
        lexbuf.Lexing.lex_curr_p <- {pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

rule token = parse
| [' ' '\t']    { token lexbuf }
| '\n'          { incr_linenum lexbuf; token lexbuf }
| eof           { EOF }
| ";"           { ENDLINE }
| "<"           { LT }
| ">"           { GT }
| "<="          { LEQ }
| ">="          { GEQ }
| "="           { EQ }
| "!="          { NEQ }
| "."           { PUNKT }
| ","           { COMA }
| "("           { LPAR }
| ")"           { RPAR }
| "+"           { ADD }
| "-"           { SUB }
| "*"           { TIMES }
| "/"           { DIV }
| ['0'-'9']+ as s   { NUMBER(int_of_string s)}
| '"'[^ '"']*'"' as s   { STRING(String.sub s 1 (String.length s - 2))}
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as s {
    match String.lowercase_ascii s with
    | "select"  -> SELECT
    | "where"   -> WHERE
    | "having"  -> HAVING
    | "from"    -> FROM
    | "group"   -> GROUP
    | "minus"   -> MINUS
    | "union"   -> UNION
    | "by"      -> BY
    | "order"   -> ORDER
    | "asc"     -> ASC
    | "desc"    -> DESC
    | "and"     -> AND
    | "or"      -> OR
    | "not"     -> NOT
    | "in"      -> IN
    | "as"      -> AS
    | "min"     -> MIN
    | "max"     -> MAX
    | "avg"     -> AVG
    | "count"   -> COUNT
    | _         -> ID(s)
}
