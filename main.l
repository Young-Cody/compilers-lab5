%option noyywrap
%{
    #include"common.h"
    #include"main.tab.hh"
    void begindomain();
    void enddomain();
    map<string, pair<vector<TreeNode *>, vector<int>>> SymbolTable;
%}

INTEGER [0-9]+
ID [[:alpha:]_][[:alpha:][:digit:]_]*
CONSTSTR \"(.|\n)*\"
CONSTCHAR \'(.|\n)?\'
EOL (\r\n|\n|\r)
WHITE [\t ]
BLOCKCOMMENT \/\*([^\*^\/]*|[\*^\/*]*|[^\**\/]*)*\*\/
LINECOMMENT \/\/[^\n]*
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

"++" return INCREMENT;
"--" return DECREMENT;
"&" return ADDRESS;
"=" return ASSIGN;
"+=" return INCREASSIGN;
"-=" return DECREASSIGN;
"*=" return MULASSIGN;
"/=" return DIVASSIGN;
"%=" return MODASSIGN;
"&=" return BANDASSIGN;
"^=" return BEORASSIGN;
"|=" return BORASSIGN;
"<<=" return SLASSIGN;
">>=" return SRASSIGN;
"+" return ADD;
"-" return MINUS;
"<<" return SL;
">>" return SR;
"&" return BAND;
"^" return BEOR;
"|" return BOR;
"~" return BCOMPLEMENT;
"?" return QUESTION;
":" return COLON;
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
"{" {begindomain();return LBRACE;}
"}" {enddomain();return RBRACE;}
"[" return LBRACKET;
"]" return RBRACKET;

{INTEGER} {
    TreeNode *node = new TreeNode(NODE_CONST);
    node->lineno = yylineno;
    node->varType = VAR_INTEGER;
    node->int_val = atoi(yytext);
    yylval = node;
    return INTEGER;
}
{ID} {
    TreeNode *node = new TreeNode(NODE_VAR);
    node->lineno = yylineno;
    node->var_name = yytext;
    yylval = node;
    return ID;
}
{CONSTSTR} {
    TreeNode *node = new TreeNode(NODE_CONST);
    node->lineno = yylineno;
    node->varType = VAR_STR;
    node->const_str_val = yytext;
    yylval = node;
    return CONSTSTR;
}
{CONSTCHAR} {
    TreeNode *node = new TreeNode(NODE_CONST);
    node->lineno = yylineno;
    node->varType = VAR_CHAR;
    node->int_val = yytext[1];
    yylval = node;
    return CONSTCHAR;
}
{EOL} yylineno++;
{WHITE}
{BLOCKCOMMENT}
{LINECOMMENT}

%%

void begindomain()
{
    for(auto &i:SymbolTable)
        i.second.second.push_back(0);
}

void enddomain()
{
    for(auto &i:SymbolTable)
    {
        for(int j = 0; j < i.second.second.back(); j++)
            i.second.first.pop_back();
        i.second.second.pop_back();
    }
}