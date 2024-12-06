module Example.FiniteAutomata.Automata.NondeterministicFiniteAutomata.NFASubarray

--
-- NFASubarray.hs: A non-deterministic finite automata that accepts sub-arrays of a particular array.
--

( nfaSubarray )
where

import FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFA

nfaSubarray :: [a] -> a -> NFA a a
nfaSubarray (s:symbols) silentSymbol = (silentSymbol:s:symbols, s:symbols, silentSymbol, transitions, [s], [silentSymbol])
  where
    constructTransitions [x] = [((x,x), silentSymbol), ((x,silentSymbol), silentSymbol)]
    constructTransitions (x:y:xs) = ((x,x),y) : ((x,silentSymbol),y) : constructTransitions (y:xs)
    transitions = constructTransitions (s:symbols)
