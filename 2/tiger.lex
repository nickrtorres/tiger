(*
 * (c) 1998 by Andrew W Appel
 * 
 * Modified by Nick Torres
 * 
 * NOTES
 * =====
 * - The 'ARRAY' token is currently unused—I'm not really sure how it's
 *   supposed to work
 * - Use of 'INITIAL' as the main state is a bit confusing
 * - The line numbers might be a little off
 *)
type pos = int
type lexresult = Tokens.token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

fun eof() = let val pos = hd(!linePos) in Tokens.EOF(pos,pos) end
%% 
%s COMMENT;
ws = [\ \t];
%%
<INITIAL>"/\*"     => (YYBEGIN COMMENT; continue ());
<INITIAL>break     => (Tokens.BREAK(yypos, yypos+5));
<INITIAL>do        => (Tokens.DO(yypos, yypos+2));
<INITIAL>else      => (Tokens.ELSE(yypos, yypos+4));
<INITIAL>end       => (Tokens.END(yypos, yypos+3));
<INITIAL>for       => (Tokens.FOR(yypos, yypos+3));
<INITIAL>fun       => (Tokens.FUNCTION(yypos, yypos+3));
<INITIAL>if        => (Tokens.IF(yypos, yypos+2));
<INITIAL>in        => (Tokens.IN(yypos, yypos+2));
<INITIAL>let       => (Tokens.LET(yypos, yypos+3));
<INITIAL>nil       => (Tokens.NIL(yypos, yypos+3));
<INITIAL>of        => (Tokens.OF(yypos, yypos+2));
<INITIAL>then      => (Tokens.THEN(yypos, yypos+4));
<INITIAL>to        => (Tokens.TO(yypos, yypos+2));
<INITIAL>type      => (Tokens.TYPE(yypos, yypos+4));
<INITIAL>var  	  => (Tokens.VAR(yypos,yypos+3));
<INITIAL>while     => (Tokens.WHILE(yypos, yypos+5));
<INITIAL>"["       => (Tokens.LBRACK(yypos, yypos+1));
<INITIAL>"]"       => (Tokens.RBRACK(yypos, yypos+1));
<INITIAL>"("       => (Tokens.LPAREN(yypos, yypos+1));
<INITIAL>")"       => (Tokens.RPAREN(yypos, yypos+1));
<INITIAL>"{"       => (Tokens.LBRACE(yypos, yypos+1));
<INITIAL>"}"       => (Tokens.RBRACE(yypos, yypos+1));


<INITIAL>"&"       => (Tokens.AND(yypos, yypos+1));
<INITIAL>"|"       => (Tokens.OR(yypos, yypos+1));
<INITIAL>">"       => (Tokens.GT(yypos, yypos+1));
<INITIAL>">="      => (Tokens.GE(yypos, yypos+2));
<INITIAL>"<"       => (Tokens.LT(yypos, yypos+1));
<INITIAL>"<="      => (Tokens.LE(yypos, yypos+2));
<INITIAL>"+"       => (Tokens.PLUS(yypos, yypos+1));
<INITIAL>"-"       => (Tokens.MINUS(yypos, yypos+1));
<INITIAL>"*"       => (Tokens.TIMES(yypos, yypos+1));
<INITIAL>"/"       => (Tokens.DIVIDE(yypos, yypos+1));
<INITIAL>"\."      => (Tokens.DOT(yypos, yypos+1));
<INITIAL>":="      => (Tokens.ASSIGN(yypos, yypos + 2));
<INITIAL>"="       => (Tokens.EQ(yypos, yypos+1));
<INITIAL>"<>"      => (Tokens.NEQ(yypos, yypos+2));
<INITIAL>":"       => (Tokens.COLON(yypos, yypos+1));
<INITIAL>";"       => (Tokens.SEMICOLON(yypos, yypos+1));
<INITIAL>","	     => (Tokens.COMMA(yypos,yypos+1));


<INITIAL>[A-Za-z]+ => (Tokens.ID((yytext), yypos, String.size yytext));
<INITIAL>[0-9]+    => (
                        (* TODO: is there a better way to get the int? *)
                        Tokens.INT(valOf (Int.fromString yytext),yypos,String.size yytext)
                    );
<INITIAL>\".*\"    => (Tokens.STRING((yytext), yypos, String.size yytext));

<INITIAL>{ws}+     => (lex());
<INITIAL>\n	     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());

<COMMENT>\n	     => (lineNum := !lineNum+1; linePos := yypos :: !linePos; continue());
<COMMENT>"\*/" => (YYBEGIN INITIAL; continue ());
<COMMENT>. => (lex());

.    => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
