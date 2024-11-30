module FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFACorrectness

--
-- NFACorrectness.hs: Non-deterministic Finite Automaton Correctness
--

-- Types
( NFACorrectness (..)
-- Functions
, nfaCorrectness
)
where

import Data.List (nub, sort, intercalate)
import FiniteAutomata.Util.Transition
import FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFA

--
-- NFACorrectness.hs types
--

data NFACorrectness a b
  = Correct
  | InvalidTransitionStates [a]
  | InvalidTransitionSymbols [b]
  | InvalidStartStates [a]
  | InvalidAcceptStates [a]
  | InvalidSilentSymbol b
  | DuplicateTransitions [Transition a b]
  | MultipleIssues [NFACorrectness a b]
  deriving Eq

instance (Show a, Show b) => Show (NFACorrectness a b) where
  show Correct = "Correct"
  show (InvalidTransitionStates qs) = "Invalid states in transitions: " ++ show qs
  show (InvalidTransitionSymbols ss) = "Invalid symbols in transitions: " ++ show ss
  show (InvalidStartStates qs) = "Invalid states in start states: " ++ show qs
  show (InvalidAcceptStates qs) = "Invalid states in accept states: " ++ show qs
  show (InvalidSilentSymbol s) = "Invalid silent symbol in symbols: " ++ show s
  show (DuplicateTransitions ts) = "Invalid duplicate transitions: " ++ show ts
  show (MultipleIssues is) = intercalate "\n- " ("Multiple issues:" : map show is)

--
-- NFACorrectness.hs functions
--

invalidTransitionStatesNFA :: (Eq a, Ord a) => NFA a b -> NFACorrectness a b
invalidTransitionStatesNFA nfa
  | null missingStates  = Correct
  | otherwise           = InvalidTransitionStates missingStates
  where
    ts = nfaTransitions nfa
    tStates = (map transitionIn ts) ++ (map transitionOut ts)
    missingStates = nub $ sort $ filter (not . (`elem` nfaStates nfa)) $ tStates

invalidTransitionSymbolsNFA :: (Eq b, Ord b) => NFA a b -> NFACorrectness a b
invalidTransitionSymbolsNFA nfa
  | length missingSymbols == 0  = Correct
  | otherwise                   = InvalidTransitionSymbols missingSymbols
  where
    tSymbols = map transitionSymbol $ nfaTransitions nfa
    tSymbolsNoSilent = filter (/= (nfaSilentSymbol nfa)) tSymbols
    missingSymbols = nub $ sort $ filter (not . (`elem` nfaSymbols nfa)) tSymbolsNoSilent

duplicateTransitionsNFA :: (Eq a, Ord a, Eq b, Ord b) => NFA a b -> NFACorrectness a b
duplicateTransitionsNFA nfa
  | null dups  = Correct
  | otherwise  = DuplicateTransitions dups
  where
    ts = nfaTransitions nfa
    count x = length $ filter (==x) ts
    dups = nub $ sort $ filter ((>1) . count) ts

invalidStartStatesNFA :: (Eq a, Ord a) => NFA a b -> NFACorrectness a b
invalidStartStatesNFA nfa
  | length bqs == 0  = Correct
  | otherwise       = InvalidStartStates bqs
  where
    states = nfaStates nfa
    isState q = q `elem` states
    bqs = nub $ sort $ filter (not . isState) (nfaStartStates nfa)

invalidAcceptStatesNFA :: (Eq a, Ord a) => NFA a b -> NFACorrectness a b
invalidAcceptStatesNFA nfa
  | length bqs == 0  = Correct
  | otherwise        = InvalidAcceptStates bqs
  where
    states = nfaStates nfa
    isState q = q `elem` states
    bqs = nub $ sort $ filter (not . isState) (nfaAcceptStates nfa)

invalidSilentSymbolNFA :: Eq b => NFA a b -> NFACorrectness a b
invalidSilentSymbolNFA nfa
  | silentSymbol `elem` (nfaSymbols nfa)  = InvalidSilentSymbol silentSymbol
  | otherwise                             = Correct
  where
    silentSymbol = nfaSilentSymbol nfa

nfaCorrectness :: (Eq a, Ord a, Eq b, Ord b) => NFA a b -> NFACorrectness a b
nfaCorrectness nfa
  | length issues == 0  = Correct
  | length issues == 1  = head issues
  | otherwise           = MultipleIssues issues
  where
    checks = [invalidTransitionStatesNFA nfa, invalidTransitionSymbolsNFA nfa, invalidStartStatesNFA nfa, invalidAcceptStatesNFA nfa, invalidSilentSymbolNFA nfa, duplicateTransitionsNFA nfa]
    issues = filter (/= Correct) checks
