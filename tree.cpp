#include"tree.h"

void TreeNode::addSibling(TreeNode * t)
{
    TreeNode *p = sibling;
    while(p->sibling)
        p = p->sibling;
    p->sibling = t;
}

void TreeNode::addChild(TreeNode *t)
{
    for(int i = 0; i < 4; i++)
        if(child[i] == nullptr)
        {
            child[i] = t;
            break;
        }
}

int nodeIdCnt = 0;

void TreeNode::genNodeId()
{
    nodeID = nodeIdCnt++;
    for(int i = 0; i < 4; i++)
        if(child[i]) child[i]->genNodeId();
    if(sibling) sibling->genNodeId();
}



void TreeNode::printAST()
{
    printNodeInfo();
    for(int i = 0; child[i] && i < 4; i++)
        child[i]->printAST();
    if(sibling)
        sibling->printAST();
}

void TreeNode::printNodeInfo()
{
    printf("@%d    ", nodeID);
    printf("lineno: %d    ", lineno);
    printType();
    printNodeConnection();
}

void TreeNode::printType()
{
    switch (nodeType)
    {
    case NODE_CONSTINT:
        printf("const int    value: %s", int_val);
        break;
    case NODE_CONSTSTR:
        printf("const string    value: %s", const_str_val);
        break;
    case NODE_CONSTCHAR:
        printf("const char    value: %c", const_char_val);
        break;
    case NODE_CONSTDECL:
        printf("const declare");
        break;
    case NODE_INITVAL:
        printf("initiate value list");
        break;
    case NODE_VARDECL:
        printf("variable declare");
        break;
    case NODE_BOOL:
        printf("bool    value: %d", bool_val);
        break;
    case NODE_VAR:
        printf("variable ID    type: ");
        switch (varType)
        {
        case VAR_INTEGER:
            printf("int");
            break;
        case VAR_VOID:
            printf("void");
            break;
        case VAR_BOOL:
            printf("bool");
            break;
        case VAR_CHAR:
            printf("char");
            break;
        default:
            break;
        }
        printf("    name: %s    ", var_name);
        break;
    case NODE_CONSTVAR:
        printf("const variable    type: ");
        switch (varType)
        {
        case VAR_INTEGER:
            printf("int");
            break;
        case VAR_VOID:
            printf("void");
            break;
        case VAR_BOOL:
            printf("bool");
            break;
        case VAR_CHAR:
            printf("char");
            break;
        default:
            break;
        }
        printf("    ");
        break;
    case NODE_EXPR:
        printf("expression    ");
        break;
    case NODE_CONSTEXPR:
        printf("const expression    ");
        break;
    case NODE_TYPE:
        printf("type: ");
        switch (varType)
        {
        case VAR_INTEGER:
            printf("int");
            break;
        case VAR_VOID:
            printf("void");
            break;
        case VAR_BOOL:
            printf("bool");
            break;
        case VAR_CHAR:
            printf("char");
            break;
        default:
            break;
        }
        printf("    ");
        break;
    case NODE_STMT:
        switch (stmtType)
        {
        case STMT_IF:
            printf("statement_if");
            break;
        case STMT_WHILE:
            printf("statement_while");
            break;
        case STMT_FOR:
            printf("statement_for");
            break;
        case STMT_CONSTDECL:
            printf("statement_const_declare");
            break;
        case STMT_VARDECL:
            printf("statement_variable_declare");
            break;
        case STMT_ASSIGN:
            printf("statement_assign");
            break;
        case STMT_PRINTF:
            printf("statement_printf");
            break;
        case STMT_SCANF:
            printf("statement_scanf");
            break;
        case STMT_BLANK:
            printf("statement_blank");
            break;
        case STMT_CONTINUE:
            printf("statement_continue");
            break;
        case STMT_BREAK:
            printf("statement_break");
            break;
        case STMT_RETURN:
            printf("statement_return");
            break;
        case STMT_EXPR:
            printf("statement_expression");
            break;
        case STMT_COMPOUND:
            printf("statement_compound");
            break;
        default:
            break;
        }
        printf("    ");
        break;
    case NODE_PROG:
        printf("program"    );
        break;
    case NODE_OP:
        switch (opType)
        {
            printf("operator ");
        case OP_EQUAL:
            printf("==");
            break;
        case OP_LESS:
            printf("<");
            break;
        case OP_GREATER:
            printf(">");
            break;
        case OP_LESSEQUAL:
            printf("<=");
            break;
        case OP_GREATEREQUAL:
            printf(">=");
            break;
        case OP_NOTEQUAL:
            printf("!=");
            break;
        case OP_ADD:
            printf("+");
            break;
        case OP_MINUS:
            printf("-");
            break;
        case OP_MUL:
            printf("*");
            break;
        case OP_DIV:
            printf("/");
            break;
        case OP_MOD:
            printf("%");
            break;
        case OP_AND:
            printf("&&");
            break;
        case OP_OR:
            printf("||");
            break;
        case OP_NOT:
            printf("!");
            break;
        case OP_UMINUS:
            printf("unary -");
            break;
        case OP_UADD:
            printf("unary +");
            break;
        case OP_FUNC:
            printf("()");
            break;
        case OP_ASSIGN:
            printf("=");
            break;
        case OP_COMMA:
            printf(",");
            break;
        case OP_INCREASSIGN:
            printf("+=");
            break;
        case OP_DECREASSIGN:
            printf("-=");
            break;
        case OP_MULASSIGN:
            printf("*=");
            break;
        case OP_DIVASSIGN:
            printf("/=");
            break;
        case OP_MODASSIGN:
            printf("%=");
            break;
        case OP_BANDASSIGN:
            printf("&=");
            break;
        case OP_BEORASSIGN:
            printf("^=");
            break;
        case OP_BORASSIGN:
            printf("|=");
            break;
        case OP_SLASSIGN:
            printf("<<=");
            break;
        case OP_SRASSIGN:
            printf(">>=");
            break;
        case OP_TERNARY_CONDITIONAL:
            printf("?:");
            break;
        case OP_BOR:
            printf("|");
            break;
        case OP_BEOR:
            printf("^");
            break;
        case OP_BAND:
            printf("&");
            break;
        case OP_SL:
            printf("<<");
            break;
        case OP_SR:
            printf(">>");
            break;
        case OP_BCOMPLEMENT:
            printf("~");
            break;
        case OP_PRE_INCREMENT:
            printf("pre ++");
            break;
        case OP_PRE_DECREMENT:
            printf("pre --");
            break;
        case OP_POST_INCREMENT:
            printf("post ++");
            break;
        case OP_POST_DECREMENT:
            printf("post --");
            break;
        
        case OP_DEREFERENCE:
            printf("*");
            break;
        case OP_ADDRESS:
            printf("&");
            break;
        default:
            break;
        }
        printf("    ");
        break;
    case NODE_LVAL:
        printf("left value    ");
        break;
    default:
        break;
    }
}

void TreeNode::printNodeConnection()
{
    if(child[0])
        printf("child: ");
    for(int i = 0; child[i] && i < 4; i++)
        printf("@%d ",child[i]->nodeID);
    if(sibling)
    {
        printf("sibling: ");
        printf("@%d ",sibling->nodeID);
    }
}