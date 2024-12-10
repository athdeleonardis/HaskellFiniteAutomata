module Example.FiniteAutomata.Automata.PushdownAutomata.DPDABrackets

--
-- A DPDA which accepts brackets of non-empty tuples of '.' and '(...)'
-- E.g.
--   Accepted: (.) (.,.) ((.),.) (.,(.)) etc
--   Denied: () (,) (.,) ((),.) etc
--

( dpdaBrackets )
where

import FiniteAutomata.Automata.PushdownAutomata.PDA
import FiniteAutomata.Automata.PushdownAutomata.DPDA

dpdaBracketsRules :: [PDARule Int Char] = [((0,'(','$'),(1,"X$")), ((1,'.','X'),(2,"X")), ((1,'(','X'),(1,"XX")), ((2,',','X'),(1,"X")), ((2,')','X'),(2,"")), ((2,'#','$'),(3,""))]

dpdaBrackets :: PDA Int Char = ([0,1,2,3], "X", "(.),", '#', dpdaBracketsRules, 0, '$', EmptyStack)
