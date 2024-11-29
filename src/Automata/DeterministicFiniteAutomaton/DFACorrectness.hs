module FiniteAutomata.Automata.DeterministicFiniteAutomaton.DFACorrectness

--
-- DFACorrectness.hs: Deterministic Finite Automaton Correctness
--

( DFACorrectness
, dfaCorrectness
)
where

import Data.List (nub, sort, intercalate)
import FiniteAutomata.Util.Transition
import FiniteAutomata.Automata.DeterministicFiniteAutomaton.DFA

--
-- DFACorrectness.hs types
--

data DFACorrectness a b
  = Correct
  | InvalidTransitionStates [a]
  | InvalidTransitionSymbols [b]
  | InvalidStartState a
  | InvalidAcceptStates [a]
  | NondeterministicDFA [(a,b)]
  | MultipleIssues [DFACorrectness a b]
  deriving Eq

instance (Show a, Show b) => Show (DFACorrectness a b) where
  show Correct = "Correct"
  show (InvalidTransitionStates qs) = "Incorrect states in transitions: " ++ show qs
  show (InvalidStartState q0) = "Incorrect start state: " ++ show q0
  show (InvalidAcceptStates as) = "Incorrect accept states: " ++ show as
  show (InvalidTransitionSymbols ss) = "Incorrect symbols in transitions: " ++ show ss
  show (NondeterministicDFA qss) = "Incorrect duplicate keys in transitions: " ++ show qss
  show (MultipleIssues is) = intercalate "\n- " ("Multiple issues:" : map show is)

--
-- DFACorrectness.hs functions
--

dfaInvalidTransitionStates :: Eq a => DFA a b -> DFACorrectness a b
dfaInvalidTransitionStates dfa
  | length missingStates > 0  = InvalidTransitionStates missingStates
  | otherwise                 = Correct
  where
    ts = dfaTransitions dfa
    tStates = (map transitionIn ts) ++ (map transitionOut ts)
    missingStates = filter (not . (`elem` (dfaStates dfa))) $ tStates

dfaInvalidTransitionSymbols :: Eq b => DFA a b -> DFACorrectness a b
dfaInvalidTransitionSymbols dfa
  | length missingSymbols > 0  = InvalidTransitionSymbols missingSymbols
  | otherwise                  = Correct
  where
    tSymbols = map transitionSymbol $ dfaTransitions dfa
    missingSymbols = filter (not . (`elem` (dfaSymbols dfa))) $ tSymbols

dfaInvalidStartState :: Eq a => DFA a b -> DFACorrectness a b
dfaInvalidStartState dfa
  | startState `elem` dfaStates dfa  = Correct
  | otherwise                        = InvalidStartState startState
  where
    startState = dfaStartState dfa

dfaInvalidAcceptStates :: Eq a => DFA a b -> DFACorrectness a b
dfaInvalidAcceptStates dfa
  | null missingAcceptStates  = Correct
  | otherwise                 = InvalidAcceptStates missingAcceptStates
  where
    missingAcceptStates = filter (not . (`elem` (dfaStates dfa))) $ dfaAcceptStates dfa

dfaNondeterministic :: (Eq a, Ord a, Eq b, Ord b) => DFA a b -> DFACorrectness a b
dfaNondeterministic dfa
  | null duplicatedPairs  = Correct
  | otherwise             = NondeterministicDFA duplicatedPairs
  where
    count x = length $ filter (==x) pairs
    pairs = map transitionInSymbol $ dfaTransitions dfa
    duplicatedPairs = nub $ sort $ filter ((>1) . count) pairs

dfaCorrectness :: (Eq a, Ord a, Eq b, Ord b) => DFA a b -> DFACorrectness a b
dfaCorrectness dfa
  | length issues == 0  = Correct
  | length issues == 1  = head issues
  | otherwise           = MultipleIssues issues
  where
    allChecks = [dfaInvalidTransitionStates dfa, dfaInvalidTransitionSymbols dfa, dfaInvalidStartState dfa, dfaInvalidAcceptStates dfa, dfaNondeterministic dfa]
    issues = filter (/= Correct) allChecks
