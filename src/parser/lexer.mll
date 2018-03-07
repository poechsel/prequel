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
| ['S''s']['E''e']['L''l']['E''e']['C''c']['T''t']  { SELECT }
| ['w''W']['h''H']['e''E']['r''R']['e''E']   { WHERE }
| ['f''F']['r''R']['o''O']['m''M']  { FROM }
| ['g''G']['r''R']['o''O']['u''U']['p''P']    { GROUP }
| ['m''M']['i''I']['n''N']['u''U']['s''S']   { MINUS }
| ['u''U']['n''N']['i''I']['o''O']['n''N']   { UNION }
| ['b''B']['y''Y']      { BY }
| ['o''O']['r''R']['d''D']['e''E']['r''R']   { ORDER }
| ['a''A']['n''N']['d''D']     { AND }
| ['o''O']['r''R']      { OR }
| ['n''N']['o''O']['t''T']     { NOT }
| ['i''I']['n''N']      { IN }
| ['a''A']['s''S']      { AS }
| "<"       { LT }
| ">"       { GT }
| "<="      { LEQ }
| ">="      { GEQ }
| "="      { EQ }
| "!="      { NEQ }
| "."       { PUNKT }
| ","       { COMA }
| "("       { LPAR }
| ")"       { RPAR }
| "+"       { ADD }
| "-"       { SUB }
| "*"       { TIMES }
| "/"       { DIV }
| ['0'-'9']+ as s   { NUMBER(int_of_string s)}
| '"'[^ '"']*'"' as s   { STRING(String.sub s 1 (String.length s - 2))}
| ['a'-'z''A'-'Z']['a'-'z''A'-'Z''0'-'9']* as s { ID(s)}
