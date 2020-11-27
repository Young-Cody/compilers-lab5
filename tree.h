#ifndef TREE_H
#define TREE_H
#include<iostream>
#include<string>

using std::cerr;
using std::cout;
using std::endl;
using std::string;

enum NodeType{
    NODE_CONSTINT,
    NODE_CONSTSTR,
    NODE_CONSTCHAR,
    NODE_CONSTDECL,
    NODE_INITVAL,
    NODE_VARDECL,
    NODE_BOOL,
    NODE_VAR,
    NODE_CONSTVAR,
    NODE_EXPR,
    NODE_TYPE,
    NODE_STMT,
    NODE_PROG,
    NODE_OP,
    NODE_LVAL
};

enum StmtType{
    STMT_IF,
    STMT_WHILE,
    STMT_FOR,
    STMT_CONSTDECL,
    STMT_VARDECL,
    STMT_ASSIGN,
    STMT_PRINTF,
    STMT_SCANF,
    STMT_BLANK,
    STMT_CONTINUE,
    STMT_BREAK,
    STMT_RETURN,
    STMT_EXPR,
    STMT_COMPOUND
};

enum OpType{
    OP_EQUAL,
    OP_LESS,
    OP_GREATER,
    OP_LESSEQUAL,
    OP_GREATEREQUAL,
    OP_NOTEQUAL,
    OP_ADD,
    OP_MINUS,
    OP_MUL,
    OP_DIV,
    OP_MOD,
    OP_AND,
    OP_OR,
    OP_NOT,
    OP_UMINUS,
    OP_UADD,
    OP_FUNC,
    OP_ASSIGN,
    OP_COMMA,
    OP_INCREASSIGN,
    OP_DECREASSIGN,
    OP_MULASSIGN,
    OP_DIVASSIGN,
    OP_MODASSIGN,
    OP_BANDASSIGN,
    OP_BEORASSIGN,
    OP_BORASSIGN,
    OP_SLASSIGN,
    OP_SRASSIGN,
    OP_TERNARY_CONDITIONAL,
    OP_BOR,
    OP_BEOR,
    OP_BAND,
    OP_SL,
    OP_SR,
    OP_BCOMPLEMENT,
    OP_PRE_INCREMENT,
    OP_PRE_DECREMENT,
    OP_POST_INCREMENT,
    OP_POST_DECREMENT,

    OP_DEREFERENCE,
    OP_ADDRESS
};

enum VarType{
    VAR_INTEGER,
    VAR_VOID,
    VAR_BOOL,
    VAR_CHAR
};

struct TreeNode {
    int nodeID;
    NodeType nodeType;
    int lineno;

    TreeNode *child[4] = {nullptr, nullptr,nullptr,nullptr};
    TreeNode *sibling = nullptr;

    void addChild(TreeNode *);
    void addSibling(TreeNode *);

    void genNodeId();//从根节点开始逐个赋Id 实现方式同学们可以自行修改

    void printAST();//打印语法树结点
    /***
     * 以下的几个函数皆为在printAST过程中辅助输出使用
     * 同学们可以根据需要自己使用其他方法
    ***/
    void printNodeInfo();
    void printNodeConnection();
    string nodeTypeInfo();

    int int_val;
    bool bool_val;
    string const_str_val;
    char const_char_val;

    StmtType stmtType;
    OpType opType;

    VarType varType;
    string var_name;

    TreeNode(NodeType type);
};
#endif