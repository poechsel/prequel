{
    open Parser;;
    exception Eof;;

let incr_linenum lexbuf =
    let pos = lexbuf.Lexing.lex_curr_p in
    lexbuf.Lexing.lex_curr_p <- {pos with
        Lexing.pos_lnum = pos.Lexing.pos_lnum + 1;
        Lexing.pos_bol = pos.Lexing.pos_cnum;
    }
}

rule token = parse
| [' ' '\t'] { token lexbuf }
| '\n' {incr_linenum lexbuf; token lexbuf}
| ";"       { ENDLINE }
| "SELECT"  { SELECT }
| "WHERE"   { WHERE }
| "FROM"    { FROM }
| "GROUP"   { GROUP }
| "MINUS"   { MINUS }
| "UNION"   { UNION }
| "BY"      { BY }
| "ORDER"   { ORDER }
| "AND"     { AND }
| "OR"      { AND }
| "NOT"     { NOT }
| "IN"      { IN }
| "AS"      { AS }
| "<"       { LT }
| ">"       { GT }
| "<="      { LEQ }
| ">="      { GEQ }
| "=="      { EQ }
| "!="      { NEQ }
| "."       { PUNKT }
| ","       { COMA }
| "("       { LPAR }
| ")"       { RPAR }
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as s { ID(s)}
