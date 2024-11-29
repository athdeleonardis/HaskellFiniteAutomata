module FiniteAutomata.Automata.DeterministicFiniteAutomaton.DFA

--
-- DFA.hs: Deterministic Finite Automaton
--

-- Types
( DFA
-- DFA Getters
, dfaStates
, dfaSymbols
, dfaTransitions
, dfaStartState
, dfaAcceptStates
-- DFA Word Checking
, dfaCheckString
)
where

import Data.Maybe (maybe, isJust, isNothing, fromJust)
import FiniteAutomata.Util.Transition

--
-- DFA.hs types
--

-- (qs,ss,ts,q0,as) represents a DFA with state names 'qs', symbols 'ss', transitions 'ts', start state 'q0', accept states 'as'
type DFA a b = ([a], [b], [Transition a b], a, [a]) 

--
-- DFA.hs functions
--

-- |Get the states of the discrete finite automaton
dfaStates :: DFA a b -> [a]
dfaStates (qs, _, _, _, _) = qs

-- |Get the symbols of the discrete finite automaton
dfaSymbols :: DFA a b -> [b]
dfaSymbols (_, ss, _, _, _) = ss

-- |Get the transitions of the discrete finite automaton
dfaTransitions :: DFA a b -> [Transition a b]
dfaTransitions (_, _, ts, _, _) = ts

-- |Get the start state of the discrete finite automaton
dfaStartState :: DFA a b -> a
dfaStartState (_, _, _, q0, _) = q0

-- |Get the accept states of the discrete finite automaton
dfaAcceptStates :: DFA a b -> [a]
dfaAcceptStates (_, _, _, _, as) = as

-- |Check the discrete finite automaton accepts the string of symbols
dfaCheckString :: (Eq a, Eq b) => DFA a b -> [b] -> Bool
dfaCheckString dfa str
  | isNothing mqf  = False
  | otherwise      = fst (fromJust mqf) `elem` dfaAcceptStates dfa
  where
    mqf = dfaSimulate dfa (dfaStartState dfa) str

dfaSimulate :: (Eq a, Eq b) => DFA a b -> a -> [b] -> Maybe (a, [b])
dfaSimulate dfa q0 str
  = until ended doStep $ Just (q0, str)
  where
    ended :: Maybe (a, [b]) -> Bool
    ended Nothing = True
    ended (Just (_, str)) = null str
    doStep = maybe Nothing (dfaStep dfa)

dfaStep :: (Eq a, Eq b) => DFA a b -> (a, [b]) -> Maybe (a, [b])
dfaStep _ (q, []) = Just (q, [])
dfaStep dfa (q, (s:ss))
  | null transitions  = Nothing
  | otherwise         = Just (transitionOut $ head transitions, ss)
  where
    transitions = filter ((==(q,s)) . transitionInSymbol) $ dfaTransitions dfa
