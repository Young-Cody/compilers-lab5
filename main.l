%option noyywrap
%{
    #include"common.h"
    #include"main.tab.hh"
    yyFlexLexer lexer;
    bool declare;
    map<string, pair<vector<int>, vector<int>>> m;
    int installID();
    void begindomain();
    void enddomain();
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
"int" {declare = true; return INT;}
"char" {declare = true; return CHAR;}
"bool" {declare = true;return BOOL;}
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

";" return SEMICOLON;
"(" return LPAREN;
")" return RPAREN;
"{" {begindomain();return LBRACE;}
"}" {enddomain();return RBRACE;}
"[" return LBRACKET;
"]" return RBRACKET;

{INTEGER} {
    TreeNode *node = new TreeNode(NODE_CONST);
    node->int_val = atoi(yytext);
    yylval = node;
    return INTEGER;
}
{ID} {
    int ptr = installID();
    TreeNode *node = new TreeNode(NODE_VAR);
    node->var_ptr = ptr;
    yylval = node;
    return ID;
}
{CONSTSTR} {
    TreeNode *node = new TreeNode(NODE_CONST);
    node->
}
{CONSTCHAR} {

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

int installID()
{
    if(declare)
    {
        if(m.find(lexer.YYText()) == m.end())
        {
            vector<int> num;
            num.push_back(0);
            m[lexer.YYText()] = {vector<int>(), num};
        }
        m[lexer.YYText()].first.push_back(signTable.size());
        m[lexer.YYText()].second.back()++;
        signTable.push_back(lexer.YYText());
        declare = false;
    }
    return m[lexer.YYText()].first.back();
}

void begindomain()
{
    for(auto &i:m)
        i.second.second.push_back(0);
}

void enddomain()
{
    for(auto &i:m)
    {
        for(int j = 0; j < i.second.second.back(); j++)
            i.second.first.pop_back();
        i.second.second.pop_back();
    }
}