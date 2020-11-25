%{
    #include"common.h"
    extern TreeNode * root;
    extern yyFlexLexer lexer;
    int yylex();
    int yyerror( char const * );
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


%right NOT
%left ADD
%left EQUAL
%right ASSIGN
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
    : IF LPAREN cond RPAREN stmt %prec LOWER_THEN_ELSE {
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
cond
    : bool_expr {$$=$1;}
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
    | RETURN expr {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_RETURN;
        node->addChild($2);
        $$ = node;
    }
    ;
asgStmt
    : lVal ASSIGN expr {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_ASSIGN;
        node->addChild($1);
        node->addChild($3);
        $$ = node;
    }
    ;
forStmt
    : 

    ;
compoundStmt
    : LBRACE stmts RBRACE {
        TreeNode *node = new TreeNode(NODE_STMT);
        node->stmtType = STMT_COMPOUND;
        node->addChild($2);
        $$ = node;
    }
instruction
    : type ID ASSIGN expr SEMICOLON {
        TreeNode *node=new TreeNode(NODE_STMT);
        node->stmtType=STMT_DECL;
        node->addChild($1);
        node->addChild($2);
        node->addChild($4);
        $$=node;
    }
    | ID ASSIGN expr SEMICOLON {
        TreeNode *node=new TreeNode(NODE_STMT);
        node->stmtType=STMT_ASSIGN;
        node->addChild($1);
        node->addChild($3);
        $$=node;  
    }
    | printf SEMICOLON {$$=$1;}
    | scanf SEMICOLON {$$=$1;}
    ;
printf
    : PRINTF LPAREN expr RPAREN {
        TreeNode *node=new TreeNode(NODE_STMT);
        node->stmtType=STMT_PRINTF;
        node->addChild($3);
        $$=node;
    }
    ;
scanf
    : SCANF LPAREN expr RPAREN {
        TreeNode *node=new TreeNode(NODE_STMT);
        node->stmtType=STMT_SCANF;
        node->addChild($3);
        $$=node;
    }
    ;
bool_expr
    : TRUE {$$=$1;}
    | FALSE {$$=$1;}
    | expr EQUAL expr {
        TreeNode *node=new TreeNode(NODE_OP);
        node->opType=OP_EQUAL;
        node->addChild($1);
        node->addChild($3);
        $$=node;
    }
    | NOT bool_expr {
        TreeNode *node=new TreeNode(NODE_OP);
        node->opType=OP_NOT;
        node->addChild($2);
        $$=node;        
    }
    ;
expr
    : ID {$$=$1;}
    | INTEGER {$$=$1;}
    | expr ADD expr {
        TreeNode *node=new TreeNode(NODE_OP);
        node->opType=OP_ADD;
        node->addChild($1);
        node->addChild($3);
        $$=node;   
    }
    ;
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
    ;

%%