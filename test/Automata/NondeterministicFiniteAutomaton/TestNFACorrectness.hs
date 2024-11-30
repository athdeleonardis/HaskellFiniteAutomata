module Test.FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFACorrectness

--
-- TestNFACorrectness.hs: Non-deterministic Finite Automaton Correctness Test
--

( main )
where

import Data.List (zip, intercalate)
import FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFA
import FiniteAutomata.Automata.NondeterministicFiniteAutomaton.NFACorrectness

nfa1 :: NFA Int Char = ([0,1,2], "abc", epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((0,epsilon),2), ((1,epsilon),2)], [1], [2])
correctness1 :: NFACorrectness Int Char = Correct

nfa2 :: NFA Int Char = ([1,2], "abc", epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((0,epsilon),2), ((1,epsilon),2)], [1], [2])
correctness2 :: NFACorrectness Int Char = InvalidTransitionStates [0]

nfa3 :: NFA Int Char = ([0,1,2], "ab", epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((0,epsilon),2), ((1,epsilon),2)], [1], [2])
correctness3 :: NFACorrectness Int Char = InvalidTransitionSymbols "c"

nfa4 :: NFA Int Char = ([0,1,2], "abc", epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((0,epsilon),2), ((1,epsilon),2)], [1,3,4], [2])
correctness4 :: NFACorrectness Int Char = InvalidStartStates [3,4]

nfa5 :: NFA Int Char = ([0,1,2], "abc", epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((0,epsilon),2), ((1,epsilon),2)], [1], [2,3,4])
correctness5 :: NFACorrectness Int Char = InvalidAcceptStates [3,4]

nfa6 :: NFA Int Char = ([0,1,2], "abc" ++ [epsilon], epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((0,epsilon),2), ((1,epsilon),2)], [1], [2])
correctness6 :: NFACorrectness Int Char = InvalidSilentSymbol epsilon

nfa7 :: NFA Int Char = ([0,1,2], "abc", epsilon, [((0,'a'),1), ((0,'b'),1), ((1,'c'),0), ((1,'c'),0), ((0,'b'),1), ((0,epsilon),2), ((1,epsilon),2)], [0,1], [2])
correctness7 :: NFACorrectness Int Char = DuplicateTransitions [((0,'b'),1), ((1,'c'),0)]

type TestCase = (NFA Int Char, NFACorrectness Int Char)

failureToStr :: TestCase -> Int -> [Char]
failureToStr (nfa, correctness) index = show index ++ "\n\tNFA: " ++ show nfa ++ "\n\t: Expected: " ++ show correctness ++ "\n\tActual: " ++ show (nfaCorrectness nfa)

printFailures :: [TestCase] -> IO ()
printFailures failures = printFailuresInternal failures 0

printFailuresInternal :: [TestCase] -> Int -> IO ()
printFailuresInternal [] index = do return ()
printFailuresInternal failures index = do
  putStrLn $ failureToStr (head failures) index
  printFailuresInternal (tail failures) (index + 1)

main :: IO ()
main = do
  let nfas :: [NFA Int Char] = [nfa1, nfa2, nfa3, nfa4, nfa5, nfa6, nfa7]
  let results :: [NFACorrectness Int Char]= [correctness1, correctness2, correctness3, correctness4, correctness5, correctness6, correctness7]
  let pairs :: [TestCase] = zip nfas results
  let failures :: [TestCase] = filter (\x -> nfaCorrectness (fst x) /= snd x) pairs
  if (length failures == 0)
  then do
    putStrLn "Test -- NFACorrectness -- Success"
  else do
    putStrLn "Test -- NFACorrectness -- Failed"
    printFailures failures
