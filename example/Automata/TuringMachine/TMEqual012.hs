module Example.FiniteAutomata.Automata.TuringMachine.TMEqual012

--
-- TMEqual012.hs: A turing machine that accepts strings (0^n)(1^n)(2^n) for n > 0
--

( tmEqual012 )
where

import FiniteAutomata.Automata.TuringMachine.TM

goDir :: (a,b,TMMove) -> ((a,b),(a,b,TMMove))
goDir (q, s, dir) = ((q, s), (q, s, dir))

tmEqual012Rules = [((0,'0'),(1,'A',TMRight)), goDir (1,'0',TMRight), goDir (1,'B',TMRight), ((1,'1'),(2,'B',TMRight)), goDir (2,'1',TMRight), goDir (2,'C',TMRight), ((2,'2'),(3,'C',TMLeft)), goDir (3,'C',TMLeft), goDir (3,'1',TMLeft), goDir (3,'B',TMLeft), goDir (3,'0',TMLeft), ((3,'A'),(0,'A',TMRight)), ((0,'B'),(4,'B',TMRight)), goDir (4,'B',TMRight), goDir (4,'C',TMRight), ((4,'X'),(5,'X',TMRight))]

tmEqual012 :: TM Int Char = ([0,1,2,3,4,5], "012ABC", 'X', "012", tmEqual012Rules, 0, [5])
