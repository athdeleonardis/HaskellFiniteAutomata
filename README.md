# HaskellFiniteAutomata

A Haskell library of various kinds of finite automata.

Finite automata are a fundamental concept in computer science. \
They are state machines which accept some set of input strings. \
It's nice to see how simply they can be defined in a functional language like Haskell.

## Finite Automata Types

- Deterministic Finite Automaton (DFA)
  > A simple directed edge graph, where states are the nodes, and directed edges are the characters of the input string.
- Non-deterministic Finite Automaton (NFA)
  > Same structure as a DFA, but allowing for multiple directed edges with the same input symbol, aswell as directed edges marked by a 'silent' symbol, i.e. not requiring input, meaning the total state is multiple nodes.
- Context Free Grammar (CFG)
  > A set of rules where single characters get expanded into strings, and strings iterate by all possible rules.
- Deterministic Pushdown Automaton (DPDA)
  > Like a DFA, but included in the state is also a stack. Edges between nodes now also pop a single element from the stack, and push a string of elements to it.
- Turing Machine (TM)
  > The input string is not read character by character, instead, it is placed into a tape (A resizable array with a pointer to an element), and edges on the graph now read at the tape pointer, replace the symbol at the tape pointer, and then move the tape pointer either left or right.

## Examples

For examples, see the test files located in 'test/', and the example files located in 'example/'
