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