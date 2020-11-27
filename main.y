%{
    #include"common.h"
    extern TreeNode * root;
    int yylex();
    int yyerror( char const * );
    TreeNode* installID(string);
    TreeNode* newOpNode(TreeNode *, TreeNode*, OpType);
    TreeNode* newExprNode(TreeNode *);
%}
%defines

%start program

%token ID INTEGER CONSTSTR CONSTCHAR
%token IF ELSE WHILE FOR
%token INT VOID CHAR BOOL
%token LPAREN RPAREN LBRACE RBRACE SEMICOLON LBRACKET RBRACKET
%token TRUE FALSE
%token ADD ASSIGN EQUAL NOT MINUS MUL DIV MOD OR AND NOTEQUAL LESS GREATER LESSEAUAL GREATEREQUAL 
%token PRINTF SCANF
%token CONST
%token RETURN CONTINUE BREAK

%left COMMA
%right ASSIGN INCREASSIGN DECREASSIGN MULASSIGN DIVASSIGN MODASSIGN BANDASSIGN BEORASSIGN BORASSIGN SLASSIGN SRASSIGN
%right TERNARY_CONDITIONAL
%left OR
%left AND
%left BOR
%left BEOR
%left BAND
%left EQUAL NOTEQUAL
%left GREATER LESS GREATEREQUAL LESSEAUAL
%left SL SR
%left ADD MINUS
%left DIV MUL MOD
%right NOT UMINUS UADD PRE_INCREMENT PRE_DECREMENT BCOMPLEMENT DEREFERENCE ADDRESS
%left FUNC LVAL POST_INCREMENT POST_DECREMENT
%nonassoc LOWER_THEN_ELSE
%nonassoc ELSE
%%
program
    : stmts {root=new TreeNode(NODE_PROG);root->addChild($1);}
    ;
stmts
    : stmt {$$=$1;}
    | stmts stmt{$$=$1;$$->addSibling($2);}
    ;
stmt
    : exprStmt {$$=$1;}
    | declStmt {$$=$1;}
    | blankStmt {$$=$1;}
    | ifStmt {$$=$1;}
    | wlStmt {$$=$1;}
    | breakStmt {$$=$1;}
    | continueStmt {$$=$1;}
    | returnStmt {$$=$1;}
    | asgStmt {$$=$1;}
    | forStmt {$$=$1;}
    | compoundStmt {$$=$1;}
    | printfStmt {$$=$1;}
    | scanfStmt {$$=$1;}
    ;
exprStmt
    : expr SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->lineno = yylineno;
        TreeNode *e = newExprNode($1);
        node->stmtType = STMT_EXPR;
        node->addChild(e);
        $$ = node;
    }
    ;
declStmt
    : constDeclStmt {$$ = $1;}
    | varDeclStmt {$$ = $1;}
    ;
blankStmt
    : SEMICOLON {
        TreeNode *node = new node(NODE_STMT);
        node->lineno = yylineno;
        node->stmtType = STMT_BLANK;
        $$ = node;
    }
    ;
ifStmt
    : IF LPAREN expr RPAREN stmt %prec LOWER_THEN_ELSE {
        TreeNode *node = new TreeNode(NODE_STMT);
        TreeNode *e = newExprNode($3);
        node->stmtType = STMT_IF;
        node->addChild(e);
        node->addChild($5);
        node->lineno = yylineno;
        $$ = node;
    }
    | IF LPAREN cond RPAREN stmt ELSE stmt {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_IF;
        node->addChild($3);
        node->addChild($5);
        node->addChild($7);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
wlStmt
    : WHILE LPAREN cond RPAREN stmt {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_WHILE;
        node->addChild($2);
        node->addChild($3);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
breakStmt
    : BREAK SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_BREAK;
        node->lineno = yylineno;
        $$ = node;
    }
    ;
continueStmt
    : CONTINUE SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_CONTINUE;
        node->lineno = yylineno;
        $$ = node;
    }
    ;
returnStmt
    : RETURN SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_RETURN;
        node->lineno = yylineno;
        $$ = node;
    }
    ;
    | RETURN expr SEMICOLON{
        TreeNode *node = new TreeNode(NODE_STMT);
        TreeNode *e = newExprNode($2);
        node->stmtType = STMT_RETURN;
        node->addChild(e);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
asgStmt
    : lVal ASSIGN expr SEMICOLON{
        TreeNode *node = new TreeNode(NODE_STMT);
        TreeNode *e = newExprNode($3);
        node->stmtType = STMT_ASSIGN;
        node->addChild($1);
        node->addChild(e);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
forStmt
    : 
    FOR LPAREN optExpr SEMICOLON optExpr SEMICOLON optExpr RPAREN stmt {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_FOR;
        node->addChild($3);
        node->addChild($5);
        node->addChild($7);
        node->addChild($9);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
compoundStmt
    : LBRACE stmts RBRACE {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_COMPOUND;
        node->addChild($2);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
printfStmt
    : PRINTF LPAREN expr RPAREN {
        TreeNode *node=new TreeNode(NODE_STMT);
        TreeNode *e = newExprNode($3);
        node->stmtType=STMT_PRINTF;
        node->addChild(e);
        node->lineno = yylineno;
        $$=node;
    }
    ;
scanfStmt
    : SCANF LPAREN expr RPAREN {
        TreeNode *node=new TreeNode(NODE_STMT);
        TreeNode *e = newExprNode($3);
        node->stmtType=STMT_SCANF;
        node->addChild(e);
        node->lineno = yylineno;
        $$=node;
    }
    ;
optExpr
    :
    expr {
        TreeNode *e = newExprNode($1);
        $$=e;}
    | {
        TreeNode *e = newExprNode(nullptr);
        $$ = node;
    }
    ;
expr
    :
    expr ADD expr {
        $$ = newOpNode($1, $3, OP_ADD);
    }
    | expr MINUS expr {
        $$ = newOpNode($1, $3, OP_MINUS);
    }
    | MINUS expr %prec UMINUS {
        $$ = newOpNode($2, nullptr, OP_UMINUS);
    }
    | INCREMENT lVal %prec PRE_INCREMENT {
        $$ = newOpNode($2, nullptr, OP_PRE_INCREMENT);
    }
    | DECREMENT lVal %prec PRE_DECREMENT {
        $$ = newOpNode($2, nullptr, OP_PRE_DECREMENT);
    }
    | lVal INCREMENT %prec POST_INCREMENT {
        $$ = newOpNode($2, nullptr, OP_POST_INCREMENT);
    }
    | lVal DECREMENT %prec POST_DECREMENT {
        $$ = newOpNode($2, nullptr, OP_POST_DECREMENT);
    }
    | MINUS expr %prec UMINUS {
        $$ = newOpNode($2, nullptr, OP_UMINUS);
    }
    | ADD expr %prec UADD {
        $$ = newOpNode($2, nullptr, OP_UADD);
    }
    | expr MUL expr {
        $$ = newOpNode($1, $3, OP_MUL);
    }
    | expr DIV expr {
        $$ = newOpNode($1, $3, OP_DIV);
    }
    | expr MOD expr {
        $$ = newOpNode($1, $3, OP_MOD);
    }
    | expr EQUAL expr {
        $$ = newOpNode($1, $3, OP_EQUAL);
    }
    | expr LESS expr {
        $$ = newOpNode($1, $3, OP_LESS);
    }
    | expr GREATER expr {
        $$ = newOpNode($1, $3, OP_GREATER);
    }
    | expr LESSEQUAL expr {
        $$ = newOpNode($1, $3, OP_LESSEQUAL);
    }
    | expr GREATEREQUAL expr {
        $$ = newOpNode($1, $3, OP_GREATEREQUAL);
    }
    | expr NOTEQUAL expr {
        $$ = newOpNode($1, $3, OP_NOTEQUAL);
    }
    | expr AND expr {
        $$ = newOpNode($1, $3, OP_AND);
    }
    | expr OR expr {
        $$ = newOpNode($1, $3, OP_OR);
    }
    | expr COMMA expr {
        $$ = newOpNode($1, $3, OP_COMMA);
    }
    | expr BOR expr {
        $$ = newOpNode($1, $3, OP_BOR);
    }
    | expr BEOR expr {
        $$ = newOpNode($1, $3, OP_BEOR);
    }
    | expr BAND expr {
        $$ = newOpNode($1, $3, OP_BAND);
    }
    | expr SL expr {
        $$ = newOpNode($1, $3, OP_SL);
    }
    | expr SR expr {
        $$ = newOpNode($1, $3, OP_SR);
    }
    | BCOMPLEMENT expr {
        $$ = newOpNode($2, nullptr, OP_BCOMPLEMENT);
    }
    | NOT expr {
        $$ = newOpNode($2, nullptr, OP_NOT);
    }
    | INTEGER {$$ = $1;}
    | TRUE {
        TreeNode *node = new TreeNode(NODE_BOOL);
        node->bool_val = true;
        node->lineno = yylineno;
        $$ = node;
    }
    | FALSE {
        TreeNode *node = new TreeNode(NODE_BOOL);
        node->bool_val = false;
        node->lineno = yylineno;
        $$ = node;
    }
    | LPAREN expr RPAREN {$$ = $1;}
    | lVal ASSIGN expr {
        $$ = newOpNode($1, $3, OP_ASSIGN);
    }
    | lVal INCREASSIGN expr {
        $$ = newOpNode($1, $3, OP_INCREASSIGN);
    }
    | lVal DECREASSIGN expr {
        $$ = newOpNode($1, $3, OP_DECREASSIGN);
    }
    | lVal MULASSIGN expr {
        $$ = newOpNode($1, $3, OP_MULASSIGN);
    }
    | lVal DIVASSIGN expr {
        $$ = newOpNode($1, $3, OP_DIVASSIGN);
    }
    | lVal MODASSIGN expr {
        $$ = newOpNode($1, $3, OP_MODASSIGN);
    }
    | lVal BANDASSIGN expr {
        $$ = newOpNode($1, $3, OP_BANDASSIGN);
    }
    | lVal BEORASSIGN expr {
        $$ = newOpNode($1, $3, OP_BEORASSIGN);
    }
    | lVal BORASSIGN expr {
        $$ = newOpNode($1, $3, OP_BORASSIGN);
    }
    | lVal SLASSIGN expr {
        $$ = newOpNode($1, $3, OP_SLASSIGN);
    }
    | lVal SRASSIGN expr {
        $$ = newOpNode($1, $3, OP_SRASSIGN);
    }
    | expr QUESTION expr COLON expr %prec TERNARY_CONDITIONAL {
        $$ = newOpNode($1, $3, OP_SRASSIGN);
        $$->addChild($5);
    }
    | ID LPAREN funcRParams RPAREN %prec FUNC{
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        $$ = newOpNode(t, $3, OP_FUNC);
    }
    | ID LPAREN RPAREN %prec FUNC{
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        $$ = newOpNode(t, nullptr, OP_FUNC);
    }
    | lVal %prec LVAL{$$ = $1;}
    ;
funcRParams
    :
    expr {
        TreeNode *e = newExprNode($1);
        $$ = e;}
    |
    funcRParams COMMA expr {
        TreeNode *e = newExprNode($3);
        $$ = $1;
        $$->addSibling(e);
    }
    ;
lVal
    :
    ID {
        TreeNode *node = new TreeNode(NODE_LVAL);
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        node->addChild(t);
        node->lineno = yylineno;
        $$ = node;
    }
    ID varBracketList {
        TreeNode *node = new TreeNode(NODE_LVAL);
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        node->addChild(t);
        node->addChild($2);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
varBracketList
    :
    LBRACKET expr RBRACKET {
        TreeNode *e = newExprNode($2);
        $$ = e;
    }
    |
    varBracketList LBRACKET expr RBRACKET {
        $$ = $1;
        TreeNode *e = newExprNode($1);
        $$->addSibling(e);
    }
    ;
type
    : INT {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_INTEGER;
        node->lineno = yylineno;
        $$=node; 
    }
    | VOID {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_VOID;
        node->lineno = yylineno;
        $$=node;         
    }
    | BOOL {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_BOOL;
        node->lineno = yylineno;
        $$=node;         
    }
    | CHAR {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_CHAR;
        node->lineno = yylineno;
        $$=node;         
    }
    ;
constDeclStmt
    :
    CONST type constDefList SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_CONSTDECL;
        node->addChild($2);
        node->addChild($3);
        node->lineno = yylineno;
        TreeNode *head = $3;
        while(head)
        {
            head->child[0]->nodeType =  NODE_CONSTVAR;
            head->child[0]->varType = $2->varType;
            head = head->sibling;
        }
    }
    ;
constDefList
    :
    constDefList COMMA constDef {
        $$ = $1;
        $$->addSibling($3);
    }
    |
    constDef {
        $$ = $1;
    }
    ;
constDef
    :
    ID ASSIGN initVal {
        TreeNode *node = new TreeNode(NODE_CONSTDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($3);
        node->lineno = yylineno;
        $$ = node;
    }
    |
    ID constBracketList ASSIGN initVal {
        TreeNode *node = new TreeNode(NODE_CONSTDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($2);
        node->addChild($4);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
constBracketList
    :
    LBRACKET expr RBRACKET{
        TreeNode *ce = new TreeNode(NODE_CONSTEXPR);
        ce->addChild($2);
        ce->lineno = $2->lineno;
        $$ = e;
    }
    constBracketList LBRACKET expr RBRACKET RBRACKET {
        TreeNode *ce = new TreeNode(NODE_CONSTEXPR);
        ce->addChild($3);
        ce->lineno = $3->lineno;
        $$ = $1;
        $$->addSibling(e);
    }
    ;
initVal
    :
    expr {
        TreeNode *node = new TreeNode(NODE_INITVAL);
        TreeNode *e = newExprNode($1);
        node->addChild(e);
        node->lineno = yylineno;
        $$ = node;
    }
    |
    LBRACE initValList RBRACE {
        TreeNode *node = new TreeNode(NODE_INITVAL);
        node->addChild($2);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
initValList
    :
    initVal {
        $$ = $1;
    }
    |
    initValList COMMA initVal {
        $$ = $1;
        $$->addSibling($3);
    }
    ;
varDeclStmt
    :
    type varDeclList SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_VARDECL;
        node->addChild($1);
        node->addChild($2);
        node->lineno = yylineno;
        $$ = node;
        TreeNode *head = $2;
        while(head)
        {
            head->child[0]->nodeType =  NODE_VAR;
            head->child[0]->varType = $1->varType;
            head = head->sibling;
        }
    }
    ;
varDeclList
    :
    varDecl {
        $$ = $1;
    }
    varDeclList COMMA varDecl {
        $$ = $1;
        $$->addSibling($3);
    }
    ;
varDecl
    :
    ID ASSIGN initVal {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($3);
        node->lineno = yylineno;
        $$ = node;
    }
    |
    ID constBracketList ASSIGN initVal {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($2);
        node->addChild($4);
        node->lineno = yylineno;
        $$ = node;
    }
    |
    ID {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->lineno = yylineno;
        $$ = node;
    }
    |
    ID constBracketList {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($2);
        node->lineno = yylineno;
        $$ = node;
    }
    ;
%%
TreeNode* installID(string id)
{
    if(SymbolTable.find(id) == SymbolTable.end())
        SymbolTable[id] = {vector<TreeNode *>(), vector<int>({0})};
    TreeNode *node = new TreeNode(NODE_VAR);
    node->var_name = id;
    node->lineno = yylineno;
    SymbolTable[id].first.push_back(node);
    SymbolTable[id].second.back()++;
    return node;
}

TreeNode* newOpNode(TreeNode *a, TreeNode *b, OpType ot)
{
    TreeNode *node = new TreeNode(NODE_OP);
    node->ot;
    node->addChild(a);
    node->addChild(b);
    node->lineno = yylineno;
    return node;
}

TreeNode* newExprNode(TreeNode *t)
{
    TreeNode *node = new TreeNode(NODE_EXPR);
    node->addChild(t);
    node->lineno = t->lineno;
    return node;
}