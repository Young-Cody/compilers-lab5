%{
    #include"common.h"
    extern TreeNode * root;
    int yylex();
    int yyerror( char const * );
    TreeNode installID(string);
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
%right ASSIGN
%left OR
%left AND
%left EQUAL NOTEQUAL
%left GREATER LESS GREATEREQUAL LESSEAUAL
%left ADD MINUS
%left DIV MUL MOD
%right NOT UMINUS UADD
%left FUNC LVAL
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
        node->stmtType = STMT_EXPR;
        node->addChild($1);
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
        node->stmtType = STMT_BLANK;
        $$ = node;
    }
ifStmt
    : IF LPAREN expr RPAREN stmt %prec LOWER_THEN_ELSE {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_IF;
        node->addChild($3);
        node->addChild($5);
        $$ = node;
    }
    | IF LPAREN cond RPAREN stmt ELSE stmt {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_IF;
        node->addChild($3);
        node->addChild($5);
        node->addChild($7);
        $$ = node;
    }
    ;
wlStmt
    : WHILE LPAREN cond RPAREN stmt {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_WHILE;
        node->addChild($2);
        node->addChild($3);
        $$ = node;
    }
    ;
breakStmt
    : BREAK SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_BREAK;
        $$ = node;
    }
    ;
continueStmt
    : CONTINUE SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_CONTINUE;
        $$ = node;
    }
    ;
returnStmt
    : RETURN SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_RETURN;
        $$ = node;
    }
    ;
    | RETURN expr SEMICOLON{
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_RETURN;
        node->addChild($2);
        $$ = node;
    }
    ;
asgStmt
    : lVal ASSIGN expr SEMICOLON{
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_ASSIGN;
        node->addChild($1);
        node->addChild($3);
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
        $$ = node;
    }
    ;
compoundStmt
    : LBRACE stmts RBRACE {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_COMPOUND;
        node->addChild($2);
        $$ = node;
    }
printfStmt
    : PRINTF LPAREN expr RPAREN {
        TreeNode *node=new TreeNode(NODE_STMT);
        node->stmtType=STMT_PRINTF;
        node->addChild($3);
        $$=node;
    }
    ;
scanfStmt
    : SCANF LPAREN expr RPAREN {
        TreeNode *node=new TreeNode(NODE_STMT);
        node->stmtType=STMT_SCANF;
        node->addChild($3);
        $$=node;
    }
    ;
optExpr
    :
    expr {$$=$1;}
    | {
        TreeNode *node = new TreeNode(NODE_EXPR);
        $$ = node;
    }
    ;
expr
    :
    expr ADD expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_ADD;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr MINUS expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_MINUS;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | MINUS expr %prec UMINUS {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_UMINUS;
        node->addChild($2);
        $$ = node;
    }
    | ADD expr %prec UADD {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_UADD;
        node->addChild($2);
        $$ = node;
    }
    | expr MUL expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_MUL;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr DIV expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_DIV;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr MOD expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_MOD;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr EQUAL expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_EQUAL;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr LESS expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_LESS;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr GREATER expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_GREATER;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr LESSEAUAL expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_LESSEAUAL;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr GREATEREQUAL expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_GREATEREQUAL;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr NOTEQUAL expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_NOTEQUAL;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr AND expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_AND;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr OR expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_OR;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | expr COMMA expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_COMMA;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | NOT expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = NOT;
        node->addChild($2);
        $$ = node;
    }
    | INTEGER {$$ = $1;}
    | TRUE {
        TreeNode *node = new TreeNode(NODE_BOOL);
        node->bool_val = true;
        $$ = node;
    }
    | FALSE {
        TreeNode *node = new TreeNode(NODE_BOOL);
        node->bool_val = false;
        $$ = node;
    }
    | LPAREN expr RPAREN {$$ = $1;}
    | lVal ASSIGN expr {
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_ASSIGN;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    | ID LPAREN funcRParams RPAREN %prec FUNC{
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_FUNC;
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        node->addChild(t);
        node->addChild($3);
        $$ = node;
    }
    | ID LPAREN RPAREN %prec FUNC{
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_FUNC;
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        node->addChild(t);
        $$ = node;
    }
    | lVal %prec LVAL{
        TreeNode *node = new TreeNode(NODE_OP);
        node->opType = OP_LVAL;
        node->addChild($1);
        $$ = node;
    }
    ;
funcRParams
    :
    expr {
        $$ = $1;
    }
    |
    funcRParams COMMA expr {
        $$ = $1;
        $$->addSibling($3);
    }
    ;
lVal
    :
    ID {
        TreeNode *node = new TreeNode(NODE_LVAL);
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        node->addChild(t);
        $$ = node;
    }
    ID varBracketList {
        TreeNode *node = new TreeNode(NODE_LVAL);
        TreeNode *t = SymbolTable[$1->var_name].first.back();
        node->addChild(t);
        node->addChild($2);
        $$ = node;
    }
    ;
varBracketList
    :
    LBRACKET expr RBRACKET {
        $$ = $1;
    }
    |
    varBracketList LBRACKET expr RBRACKET {
        $$ = $1;
        $$->addSibling($3);
    }
type
    : INT {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_INTEGER;
        $$=node; 
    }
    | VOID {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_VOID;
        $$=node;         
    }
    | BOOL {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_BOOL;
        $$=node;         
    }
    | CHAR {
        TreeNode *node=new TreeNode(NODE_TYPE);
        node->varType=VAR_CHAR;
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
        $$ = node;
    }
    |
    ID constBracketList ASSIGN initVal {
        TreeNode *node = new TreeNode(NODE_CONSTDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($2);
        node->addChild($4);
        $$ = node;
    }
constBracketList
    :
    LBRACKET expr RBRACKET{
        $$ = $1;
    }
    constBracketList LBRACKET expr RBRACKET RBRACKET {
        $$ = $1;
        $$->addSibling($3);
    }
initVal
    :
    expr {
        TreeNode *node = new TreeNode(NODE_INItVAL);
        node->addChild($1);
        $$ = node;
    }
    |
    LBRACE initValList RBRACE {
        TreeNode *node = new TreeNode(NODE_INITVAL);
        node->addChild($2);
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
varDeclStmt
    :
    type varDeclList SEMICOLON {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_VARDECL;
        node->addChild($1);
        node->addChild($2);
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
        $$ = node;
    }
    |
    ID constBracketList ASSIGN initVal {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($2);
        node->addChild($4);
        $$ = node;
    }
    |
    ID {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        $$ = node;
    }
    |
    ID constBracketList {
        TreeNode *node = new TreeNode(NODE_VARDECL);
        TreeNode *t = installID($1->var_name);
        node->addChild(t);
        node->addChild($2);
        $$ = node;
    }
%%
TreeNode* installID(string id)
{
    if(SymbolTable.find(id) == SymbolTable.end())
        SymbolTable[id] = {vector<TreeNode *>(), vector<int>({0})};
    TreeNode *node = new TreeNode(NODE_VAR);
    node->var_name = id;
    SymbolTable[id].first.push_back(node);
    SymbolTable[id].second.back()++;
    return node;
}