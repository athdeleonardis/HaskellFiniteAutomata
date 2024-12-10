module Example.FiniteAutomata.Automata.PushdownAutomata.DPDADistributedMultiplication

--
-- A DPDA that accepts formula that is the intersection of:
-- 1. Brackets must contain an addition, e.g. (a) fails, (a+b) accepts, (a*b) fails, (a*(b+c)) fails, (a+b*c) accepts, etc.
-- 2. Brackets must be part of a multiplication, e.g. (a) fails, (a)*b accepts, a*(b) accepts, a*b*(c) accepts, etc.
-- 3. Single letter variable names
-- 4. All multiplications are shown with '*'
--

( dpdaDistributedMultiplication )
where

import FiniteAutomata.Automata.PushdownAutomata.PDA

type Rule = PDARule Int Char

dpdaDMVariableRules :: Char -> [Rule]
dpdaDMVariableRules var = [((0,var,'$'),(1,"$")), ((0,var,'t'),(1,"")), ((0,var,'T'),(1,""))]

dpdaDMVariableStateRules :: [Char] -> [Rule]
dpdaDMVariableStateRules vars = concat ([((0,'(','$'),(0,"tpb")), ((0,'(','t'),(0,"tpb")), ((0,'(','T'),(0,"tpB"))] : map dpdaDMVariableRules vars)

dpdaDMInputPlusRules :: [Rule]
dpdaDMInputPlusRules = [((1,'+','$'),(0,"t$")), ((1,'+','p'),(0,"t")), ((1,'+','b'),(0,"t)")), ((1,'+','B'),(0,"tB"))]

dpdaDMInputTimesRules :: [Rule]
dpdaDMInputTimesRules = [((1,'*','$'),(0,"T$")), ((1,'*','p'),(0,"T+")), ((1,'*','x'),(0,"T")), ((1,'*','b'),(0,"Tb")), ((1,'*','B'),(0,"TB"))]

dpdaDMOperatorStateRules :: [Rule]
dpdaDMOperatorStateRules = [((1,')','b'),(1,"x")), ((1,')','B'),(1,"")), ((1,' ','$'),(2,""))] ++ dpdaDMInputPlusRules ++ dpdaDMInputTimesRules

dpdaDistributedMultiplication :: String -> PDA Int Char
dpdaDistributedMultiplication vars = ([0,1,2], "tTbBpx", vars ++ "()+*", ' ', rules, 0, '$', EmptyStack)
  where
    rules = dpdaDMOperatorStateRules ++ dpdaDMVariableStateRules vars
