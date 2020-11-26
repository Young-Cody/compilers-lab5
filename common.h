#ifndef COMMON_H
#define COMMON_H

#include "tree.h"
#include <map>
#include <vector>
using std::map;
using std::vector;
#define YYSTYPE TreeNode *

map<string, pair<vector<TreeNode *>, int> SymbolTable;

#endif