module FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFAtoDFA

(nfaToDFA

)
where

import Data.List (nub, sort)
import FiniteAutomata.Util.Transition
import FiniteAutomata.Automata.DeterministicFiniteAutomaton.DFA
import FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFA

nfaToDFA :: (Eq a, Ord a, Eq b) => NFA a b -> DFA [a] b
nfaToDFA nfa = dfa
  where
    (allStates, allTransitions) = nfaToDFAInternal nfa [] [nfaInitialState nfa] []
    startState = nfaInitialState nfa
    acceptStates = filter (any (`elem` nfaAcceptStates nfa)) allStates
    dfa = (allStates, nfaSymbols nfa, allTransitions, startState, acceptStates)


allTransitions :: (Eq a, Ord a, Eq b) => NFA a b -> [a] -> [Transition [a] b]
allTransitions nfa qs = ts
  where
    symbolToTransition s = ((qs, s), nfaStep nfa qs s)
    ts = filter (not . null . transitionOut) $ map symbolToTransition $ nfaSymbols nfa

nfaToDFAInternal :: (Eq a, Ord a, Eq b) => NFA a b -> [[a]] -> [[a]] -> [Transition [a] b] -> ([[a]], [Transition [a] b])
nfaToDFAInternal _ allStates [] allTransitions = (allStates, allTransitions)
nfaToDFAInternal nfa done toDo transitions = nfaToDFAInternal nfa newDone newToDo (transitions ++ newTransitions)
  where
    newTransitions = concat $ map (allTransitions nfa) toDo
    newToDo = filter (not . (`elem` done)) $ nub $ sort $ map transitionOut newTransitions -- toDo guarenteed to not be in done
    newDone = done ++ toDo -- toDo guarenteed to not be in done.
