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
-- NFA Functions
, nfaStep
, nfaSimulate
, nfaStringCheck
)
where

import Data.List (nub, sort)
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

nfaTransitionEndsFromKey :: (Eq a, Eq b) => NFA a b -> b -> a -> [a]
nfaTransitionEndsFromKey nfa symbol state = map transitionOut $ filter matchesKey $ nfaTransitions nfa
  where
    key = (state, symbol)
    matchesKey = (==key) . fst

nfaStep :: (Eq a, Ord a, Eq b) => NFA a b -> [a] -> b -> [a]
nfaStep nfa states symbol = nub $ sort $ nfaStepSilent nfa $ nfaStepNonSilent nfa states symbol

nfaStepNonSilent :: (Eq a, Ord a, Eq b) => NFA a b -> [a] -> b -> [a]
nfaStepNonSilent nfa states symbol = nub $ sort $ concat $ map (nfaTransitionEndsFromKey nfa symbol) states

nfaStepSilent :: (Eq a, Ord a, Eq b) => NFA a b -> [a] -> [a]
nfaStepSilent nfa states = nfaStepSilentInternal nfa states [] states

nfaStepSilentInternal :: (Eq a, Ord a, Eq b) => NFA a b -> [a] -> [a] -> [a] -> [a]
nfaStepSilentInternal _ states _ [] = states
nfaStepSilentInternal nfa states done toDo = nfaStepSilentInternal nfa (states ++ newStates) newDone newToDo
  where
    newStates = nfaStep nfa toDo $ nfaSilentSymbol nfa
    newToDo = filter (not . (`elem` done)) newStates
    newDone = nub $ sort (done ++ toDo)

nfaSimulate :: (Eq a, Ord a, Eq b) => NFA a b -> [b] -> [a] -> [a]
nfaSimulate _ _ [] = []
nfaSimulate _ [] qs = qs
nfaSimulate nfa (s:ss) qs = nfaSimulate nfa ss (nfaStep nfa qs s)

nfaStateIsAccept :: Eq a => NFA a b -> [a] -> Bool
nfaStateIsAccept nfa qs = any (`elem` (nfaAcceptStates nfa)) qs

nfaStringCheck :: (Eq a, Ord a, Eq b) => NFA a b -> [b] -> Bool
nfaStringCheck nfa str = nfaStateIsAccept nfa $ nfaSimulate nfa str $ nfaStepSilent nfa $ nfaStartStates nfa
