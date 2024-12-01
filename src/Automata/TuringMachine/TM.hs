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

data TMMove = Left | Right

type TMRuleKey a b = (a, b)
type TMRuleVal a b = (a, b, TMMove)
type TMRule a b = (TMRuleKey a b, TMRuleVal a b)

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
tmStartState (_,_,_,_,_,q0,fs) = q0

tmFinalStates :: TM a b -> [a]
tmFinalStates (_,_,_,_,_,_,fs) = fs
