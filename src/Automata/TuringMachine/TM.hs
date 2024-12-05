module FiniteAutomata.Automata.TuringMachine.TM

( TM
, TMMove (..)
, tmStates
, tmTapeSymbols
, tmBlankSymbol
, tmInputSymbols
, tmRules
, tmStartState
, tmFinalStates
)
where

import FiniteAutomata.Automata.TuringMachine.TMTape

data TMMove = TMLeft | TMRight

instance Show TMMove where
  show TMLeft = "Left"
  show TMRight = "Right"

type TMRuleKey a b = (a, b)
type TMRuleVal a b = (a, b, TMMove)
type TMRule a b = (TMRuleKey a b, TMRuleVal a b)

type TMState a b = (a, TMTape b)

-- |Turing Machine. (qs, ts, b, is, rs, q0, fs) represents a TM with states 'qs', tape symbols 'ts', tape blank symbol 'b', tape input symbols 'as', rules 'rs', start state 'q0', and final states 'fs'
type TM a b = ([a], [b], b, [b], [TMRule a b], a, [a])

tmStates :: TM a b -> [a]
tmStates (qs,_,_,_,_,_,_) = qs

tmTapeSymbols :: TM a b -> [b]
tmTapeSymbols (_,ts,_,_,_,_,_) = ts

tmBlankSymbol :: TM a b -> b
tmBlankSymbol (_,_,b,_,_,_,_) = b

tmInputSymbols :: TM a b -> [b]
tmInputSymbols (_,_,_,is,_,_,_) = is

tmRules :: TM a b -> [TMRule a b]
tmRules (_,_,_,_,rs,_,_) = rs

tmStartState :: TM a b -> a
tmStartState (_,_,_,_,_,q0,_) = q0

tmFinalStates :: TM a b -> [a]
tmFinalStates (_,_,_,_,_,_,fs) = fs

tmRuleValueFromKey :: (Eq a, Eq b) => TM a b -> TMRuleKey a b -> Maybe (TMRuleVal a b)
tmRuleValueFromKey tm key
  | length rules == 0  = Nothing
  | otherwise          = Just (snd $ head rules)
  where
    rules = filter ((==key) . fst) $ tmRules tm

tmMoveTape :: TMMove -> TMTape a -> TMTape a
tmMoveTape TMLeft tape = tmTapeLeft tape
tmMoveTape TMRight tape = tmTapeRight tape

tmStepApplyRule :: TMState a b -> Maybe (TMRuleVal a b) -> Maybe (TMState a b)
tmStepApplyRule _ Nothing = Nothing
tmStepApplyRule (q, tape) (Just (nq, symbol, move)) = Just (nq, tmMoveTape move $ tmTapeChangeValue tape symbol)

tmStep :: (Eq a, Eq b) => TM a b -> Maybe (TMState a b) -> Maybe (TMState a b)
tmStep _ Nothing = Nothing
tmStep tm (Just state) = tmStepApplyRule state ruleVal
  where
    q = fst state
    symbol = tmTapeValue $ snd state
    key = (q, symbol)
    ruleVal = tmRuleValueFromKey tm key

tmInitialState :: TM a b -> [b] -> TMState a b
tmInitialState tm str = (tmStartState tm, tmTape str (tmBlankSymbol tm))

tmSimulate :: (Eq a, Eq b) => TM a b -> [b] -> Maybe (TMState a b)
tmSimulate tm str = until ended doStep $ Just $ tmInitialState tm str
  where
    doStep = tmStep tm
    ended Nothing = True
    ended (Just (q, _)) = q `elem` (tmFinalStates tm)

tmCheckString :: (Eq a, Eq b) => TM a b -> [b] -> Bool
tmCheckString tm str = let finalState = tmSimulate tm str in
  case finalState of
    Nothing -> False
    Just _ -> True
