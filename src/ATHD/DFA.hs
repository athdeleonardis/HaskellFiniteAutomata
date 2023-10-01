module ATHD.DFA

-- DFA.hs (Deterministic Finite Automaton)

-- Types
( Input
, Transition
, DFA
-- DFA Getters
, statesDFA
, symbolsDFA
, transitionsDFA
, startStateDFA
, acceptStatesDFA
-- Transition Getters
, tIn
, tOut
, tSymbol
-- DFA Validity Checking
, DFACorrectness
, checkCorrectDFA
)
where

import Data.List (nub, sort, intercalate)

-- Types
type Input a = [a]
-- ((q1,s),q2) represents a directed edge from 'q1' to 'q2' with symbol 's'
type Transition a b = ((a, b), a)
-- (qs,ss,ts,q0,as) represents a DFA with state names 'qs', symbols 'ss', transitions 'ts', start state 'q0', accept states 'as'
type DFA a b = ([a], [b], [Transition a b], a, [a])

-- DFA Getters
statesDFA :: DFA a b -> [a]
statesDFA (qs, _, _, _, _) = qs

symbolsDFA :: DFA a b -> [b]
symbolsDFA (_, ss, _, _, _) = ss

transitionsDFA :: DFA a b -> [Transition a b]
transitionsDFA (_, _, ts, _, _) = ts

startStateDFA :: DFA a b -> a
startStateDFA (_, _, _, q0, _) = q0

acceptStatesDFA :: DFA a b -> [a]
acceptStatesDFA (_, _, _, _, as) = as

-- Transition Getters
tIn :: Transition a b -> a
tIn ((q1,_),_) = q1

tOut :: Transition a b -> a
tOut ((_,_),q2) = q2

tSymbol :: Transition a b -> b
tSymbol ((_,s),_) = s

tInSymbol :: Transition a b -> (a,b)
tInSymbol ((q1,s),_) = (q1,s)

-- DFA Validity Checking
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
  show Correct = "DFA is correct"
  show (InvalidTransitionStates qs) = "The following states exist in the transition list but not in the list of states: " ++ show qs
  show (InvalidStartState q0) = "The following start state is not in the list of states: " ++ show q0
  show (InvalidAcceptStates as) = "The following accept states are not in the list of states: " ++ show as
  show (InvalidTransitionSymbols ss) = "The following symbols exist in the transition list but not in the list of symbols: " ++ show ss
  show (NondeterministicDFA qss) = "The following state-symbol pairs are duplicated in the transition list: " ++ show qss
  show (MultipleIssues is) = intercalate "\n" ("The following issues exist in this DFA:":map show is)

invalidTransitionStatesDFA :: Eq a => DFA a b -> DFACorrectness a b
invalidTransitionStatesDFA dfa
  | length missingStates > 0  = InvalidTransitionStates missingStates
  | otherwise                 = Correct
  where
    ts = transitionsDFA dfa
    tStates = (map tIn ts) ++ (map tOut ts)
    missingStates = filter (not . (`elem` (statesDFA dfa))) $ tStates

invalidTransitionSymbolsDFA :: Eq b => DFA a b -> DFACorrectness a b
invalidTransitionSymbolsDFA dfa
  | length missingSymbols > 0  = InvalidTransitionSymbols missingSymbols
  | otherwise                  = Correct
  where
    tSymbols = map tSymbol $ transitionsDFA dfa
    missingSymbols = filter (not . (`elem` (symbolsDFA dfa))) $ tSymbols

invalidStartStateDFA :: Eq a => DFA a b -> DFACorrectness a b
invalidStartStateDFA dfa
  | startState `elem` statesDFA dfa  = Correct
  | otherwise                        = InvalidStartState startState
  where
    startState = startStateDFA dfa

invalidAcceptStatesDFA :: Eq a => DFA a b -> DFACorrectness a b
invalidAcceptStatesDFA dfa
  | null missingAcceptStates  = Correct
  | otherwise                 = InvalidAcceptStates missingAcceptStates
  where
    missingAcceptStates = filter (not . (`elem` (statesDFA dfa))) $ acceptStatesDFA dfa

nondeterministicDFA :: (Eq a, Ord a, Eq b, Ord b) => DFA a b -> DFACorrectness a b
nondeterministicDFA dfa
  | null duplicatedPairs  = Correct
  | otherwise             = NondeterministicDFA duplicatedPairs
  where
    count x = length $ filter (==x) pairs
    pairs = map tInSymbol $ transitionsDFA dfa
    duplicatedPairs = nub $ sort $ filter ((>1) . count) pairs

checkCorrectDFA :: (Eq a, Ord a, Eq b, Ord b) => DFA a b -> DFACorrectness a b
checkCorrectDFA dfa
  | length issues == 0  = Correct
  | length issues == 1  = head issues
  | otherwise           = MultipleIssues issues
  where
    allChecks = [invalidTransitionStatesDFA dfa, invalidTransitionSymbolsDFA dfa, invalidStartStateDFA dfa, invalidAcceptStatesDFA dfa, nondeterministicDFA dfa]
    issues = filter (/= Correct) allChecks
