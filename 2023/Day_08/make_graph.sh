#!/bin/sh

EDGES=`sed -E 's/(.{3}) = \((.{3}), (.{3})\)/\1 -> \2; \1 -> \3/' $1 | tail +2`
START=`grep -o -E '..A =' $1 | sed -E 's/(.{3}) =/\1 [fillcolor="green" style="filled"]/'`
END=`grep -o -E '..Z =' $1 | sed -E 's/(.{3}) =/\1 [fillcolor="red" style="filled"]/'`

echo "
digraph my_graph {
    $START
    $END
    $EDGES
}
" | neato -Tpng > graph.png
