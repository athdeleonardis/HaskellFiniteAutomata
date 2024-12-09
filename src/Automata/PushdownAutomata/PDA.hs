module FiniteAutomata.Automata.PushdownAutomata.PDA

( PDA
, PDAAccept (..)
, PDAState
, PDARule, PDARuleKey, PDARuleValue
, pdaStates
, pdaStackSymbols
, pdaInputSymbols
, pdaInputEmptySymbol
, pdaRules
, pdaStartState
, pdaStartStackSymbol
, pdaAcceptanceCriteria
)
where

import Data.List (find)

-- |PDARuleKey. (q, a, A) represents a PDARuleKey with prior state 'q', input symbol 'a' and stack symbol to pop 'A'.
type PDARuleKey a b = (a, b, b)
-- |PDARuleValue. (q, A[]) represents a PDARuleValue with post state 'q', and stack symbols to push 'A[]'.
type PDARuleValue a b = (a, [b])
type PDARule a b = (PDARuleKey a b, PDARuleValue a b)

-- |PDAState. (q, s[]) represents a PDAState with current state 'q' and stack 's[]'.
type PDAState a b = (a, [b])

data PDAAccept a = EmptyStack | AcceptStates [a]
  deriving Show

-- |Deterministic Pushdown Automata. (qs,ss,is,ε,rs,q0,s0,as) represent a PDA with states 'qs', stack symbols 'ss', input symbols 'is', input empty symbol 'ε', rules 'rs', initial state 'q0', initial stack symbol 's0', and accept states 'as'.
type PDA a b = ([a], [b], [b], b, [PDARule a b], a, b, PDAAccept a)

pdaStates :: PDA a b -> [a]
pdaStates (qs,_,_,_,_,_,_,_) = qs

pdaStackSymbols :: PDA a b -> [b]
pdaStackSymbols (_,ss,_,_,_,_,_,_) = ss

pdaInputSymbols :: PDA a b -> [b]
pdaInputSymbols (_,_,is,_,_,_,_,_) = is

pdaInputEmptySymbol :: PDA a b -> b
pdaInputEmptySymbol (_,_,_,ie,_,_,_,_) = ie

pdaRules :: PDA a b -> [PDARule a b]
pdaRules (_,_,_,_,rs,_,_,_) = rs

pdaStartState :: PDA a b -> a
pdaStartState (_,_,_,_,_,q0,_,_) = q0

pdaStartStackSymbol :: PDA a b -> b
pdaStartStackSymbol (_,_,_,_,_,_,s0,_) = s0

pdaAcceptanceCriteria :: PDA a b -> PDAAccept a
pdaAcceptanceCriteria (_,_,_,_,_,_,_,a) = a
