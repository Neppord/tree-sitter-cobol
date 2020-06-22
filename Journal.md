# 2020-06-22
Today I learnt that tree-sitter don't like large rules. That if you have a lot of optional in seq in choice in the same rule it will take ages to generate the parser (over 30 min).