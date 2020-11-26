%option noyywrap
%{
    #include"common.h"
    #include"main.tab.hh"
%}

INTEGER [0-9]+
ID [[:alpha:]_][[:alpha:][:digit:]_]*
CONSTSTR \"[.|\n]*\"
CONSTCHAR '[.|\n]'
EOL (\r\n|\n)
WHITE [\t ]
mulcombeg "/*"
mulcomele .|\n
mulcomend "*/"
sigcombeg "//"
sigcomele .
sigcomend \n
%x MCOMMENT
%x SCOMMENT
%%

"true" return TRUE;
"false" return FALSE;
"int" return INT;
"char" return CHAR;
"bool" return BOOL;
"void" return VOID;
"const" return CONST;

"if" return IF;
"while" return WHILE;
"else" return ELSE;
"for" return FOR;
"return" return RETURN;
"continue" return CONTINUE;
"break" return BREAK;
"printf" return PRINTF;
"scanf" return SCANF;

"=" return ASSIGN;
"+" return ADD;
"-" return MINUS;
"*" return MUL;
"/" return DIV;
"%" return MOD;

"!" return NOT;
"||" return OR;
"&&" return AND;
"!=" return NOTEQUAL;
"==" return EQUAL;
"<" return LESS;
">" return GREATER;
"<=" return LESSEQUAL;
">=" return GREATEREQUAL;

"," return COMMA;
";" return SEMICOLON;
"(" return LPAREN;
")" return RPAREN;
"{" return LBRACE;
"}" return RBRACE;
"[" return LBRACKET;
"]" return RBRACKET;

{INTEGER} {
    TreeNode *node = new TreeNode(NODE_CONSTINT);
    node->int_val = atoi(yytext);
    yylval = node;
    return INTEGER;
}
{ID} {
    TreeNode *node = new TreeNode(NODE_VAR);
    node->var_name = yytext;
    yylval = node;
    return ID;
}
{CONSTSTR} {
    TreeNode *node = new TreeNode(NODE_CONSTSTR);
    node->const_str_val = yytext;
    yylval = node;
    return CONSTSTR;
}
{CONSTCHAR} {
    TreeNode *node = new TreeNode(NODE_CONSTCHAR);
    node->const_char_val = yytext;
    yylval = node;
    return CONSTCHAR;
}
{EOL}
{WHITE}
{mulcombeg} {BEGIN MCOMMENT;}
<MCOMMENT>{mulcomele} {}
<MCOMMENT>{mulcomend} {BEGIN INITIAL;}
{sigcombeg} {BEGIN SCOMMENT;}
<SCOMMENT>{sigcomele} {}
<SCOMMENT>{sigcomend} {BEGIN INITIAL;}

%%