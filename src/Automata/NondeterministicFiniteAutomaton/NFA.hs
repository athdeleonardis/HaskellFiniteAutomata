module FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFA

--
-- NFA.hs: Non-deterministic Finite Automaton
--

-- Types
( NFA
-- Constants
, epsilon
-- NFA Getters
, nfaStates
, nfaSymbols
, nfaSilentSymbol
, nfaTransitions
, nfaStartStates
, nfaAcceptStates
)
where

import FiniteAutomata.Util.Transition

--
-- NFA.hs constants
--

epsilon :: Char
epsilon = 'ε'

--
-- NFA.hs types
--

-- |Non-deterministic Finite Automaton. (qs, ss, ε, ts, q0s, as) represents an NFA with state names 'qs', symbols 'ss', silent symbol 'ε', transitions 'ts', start states 'q0s', accept states 'as'
type NFA a b = ([a], [b], b, [Transition a b], [a], [a])

--
-- NFA.hs functions
--

nfaStates :: NFA a b -> [a]
nfaStates (qs,_,_,_,_,_) = qs

nfaSymbols :: NFA a b -> [b]
nfaSymbols (_,ss,_,_,_,_) = ss

nfaSilentSymbol :: NFA a b -> b
nfaSilentSymbol (_,_,s,_,_,_) = s

nfaTransitions :: NFA a b -> [Transition a b]
nfaTransitions (_,_,_,ts,_,_) = ts

nfaStartStates :: NFA a b -> [a]
nfaStartStates (_,_,_,_,q0s,_) = q0s

nfaAcceptStates :: NFA a b -> [a]
nfaAcceptStates (_,_,_,_,_,as) = as
