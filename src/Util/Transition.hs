module FiniteAutomata.Util.Transition

( Transition
, transitionIn
, transitionOut
, transitionSymbol
, transitionInSymbol
)
where

-- |((q1,s),q2) represents a directed edge from node 'q1' to node 'q2' with symbol 's'
type Transition a b = ((a, b), a)

-- 
transitionIn :: Transition a b -> a
transitionIn ((q1,_),_) = q1

transitionOut :: Transition a b -> a
transitionOut ((_,_),q2) = q2

transitionSymbol :: Transition a b -> b
transitionSymbol ((_,s),_) = s

transitionInSymbol :: Transition a b -> (a,b)
transitionInSymbol ((q1,s),_) = (q1,s)
