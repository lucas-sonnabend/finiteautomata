# Finite Automata

This is a scala library to generate nondeterministic finite automata (NFA) and deterministic finite automata (DFA) from a given regex. Both can then be used to test whether the orginial regex would accept a given input string. NFAs can be converted into DFAs and DFAs can be printed as a tikz picture.


This project is still work in progress!

## get the jar

At the moment the jar is not available in a public maven repository but you can
* install maven (if you haven't already)
* update the `publish_local.sh` script to point to your local repository
* run the `publish_local.sh` script
