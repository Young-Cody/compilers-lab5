#ifndef COMMON_H
#define COMMON_H

#include "tree.h"
#include <map>
#include <vector>
using std::map;
using std::vector;
using std::pair;
#define YYSTYPE TreeNode *

extern map<string, pair<vector<TreeNode *>, vector<int>>> SymbolTable;
extern int yylineno;

#endif