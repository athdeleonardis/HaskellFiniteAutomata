module FiniteAutomata.Automata.TuringMachine.TMCorrectness

--
-- TMCorrectness.hs: Turing Machine Correctness
--

-- Types
( TMCorrectness
-- Functions
, tmCorrectness
)
where

import FiniteAutomata.Automata.TuringMachine.TM

--
-- TMCorrectness.hs types
--

data TMCorrectness a b
  = Correct

--
-- TMCorrectness.hs functions
--

tmCorrectness :: TM a b -> TMCorrectness a b
tmCorrectness = Correct
