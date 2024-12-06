module FiniteAutomata.Automata.DeterministicPushdownAutomata.DPDA

( DPDA
, dpdaStates
, dpdaStackSymbols
, dpdaInputSymbols
, dpdaSilentSymbol
, dpdaRules
, dpdaStartState
, dpdaStartStackSymbol
, dpdaAcceptStates
)
where

type DPDARule a b = ((a, b, b), (a, [b]))

-- |Deterministic Pushdown Automata. (qs,ss,is,ε,rs,q0,s0,as) represent a DPDA with states 'qs', stack symbols 'ss', input symbols 'is', silent symbol 'ε', rules 'rs', initial state 'q0', initial stack symbol 's0', and accept states 'as'.
type DPDA a b = ([a], [b], [b], b, [DPDARule a b], a, b, [a])

dpdaStates :: DPDA a b -> [a]
dpdaStates (qs,_,_,_,_,_,_,_) = qs

dpdaStackSymbols :: DPDA a b -> [b]
dpdaStackSymbols (_,ss,_,_,_,_,_,_) = ss

dpdaInputSymbols :: DPDA a b -> [b]
dpdaInputSymbols (_,_,is,_,_,_,_,_) = is

dpdaSilentSymbol :: DPDA a b -> b
dpdaSilentSymbol (_,_,_,s,_,_,_,_) = s

dpdaRules :: DPDA a b -> [DPDARule a b]
dpdaRules (_,_,_,_,rs,_,_,_) = rs

dpdaStartState :: DPDA a b -> a
dpdaStartState (_,_,_,_,_,q0,_,_) = q0

dpdaStartStackSymbol :: DPDA a b -> b
dpdaStartStackSymbol (_,_,_,_,_,_,s0,_) = s0

dpdaAcceptStates :: DPDA a b -> [a]
dpdaAcceptStates (_,_,_,_,_,_,_,as) = as
