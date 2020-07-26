#!/usr/bin/env bash
function has_conflicts {
  npm start -s 2>&1 | grep -q "Add a conflict for these rules:"
}
function get_conflicts {
  npm start -s 2>&1 |
  grep "Add a conflict for these rules:" |
  cut -d ':' -f 3
}
function format_conflicts {
  sed - -e 's/ `/ $./g' -e 's/`//g'
}

while has_conflicts; do
  time sed -i \
    -e "s@//HERE@[$( get_conflicts | format_conflicts )],\n//HERE@" \
    grammar.js
done