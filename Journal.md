# 2020-07-06

Webstorm crashes during writing journal entries, losing the whole entry, save more often while typing.

Writing UPPER_CASE rules for keywords together with their companion words like `IS` etc both makes the code more readable and compiling the grammar more performant.

Some rules do now include "holes" like `data_name` and some of them exclude them so that their parent rules include them instead. This might be a good idea to elevate the knowledge of these "holes" to a higher level, and make them explicit higher up. 

# 2020-06-22
Today I learnt that tree-sitter don't like large rules. That if you have a lot of optional in seq in choice in the same rule it will take ages to generate the parser (over 30 min).