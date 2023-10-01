module ATHD.NFA

-------------------------------------------------
-- NFA.hs (Non-deterministic Finite Automaton) --
-------------------------------------------------

-- Types
( NFA
-- NFA Getters
, statesNFA
, symbolsNFA
, silentSymbolNFA
, transitionsNFA
, startStatesNFA
, acceptStatesNFA
-- Constants
, epsilon
)
where

import ATHD.DFA

-----------
-- Types --
-----------

-- (qs, ss, ε, ts, q0s, as) represents an NFA with state names 'qs', symbols 'ss', silent symbol 'ε', transitions 'ts', start states 'q0s', accept states 'as'
type NFA a b = ([a], [b], b, [Transition a b], [a], [a])

-----------------
-- NFA Getters --
-----------------

statesNFA :: NFA a b -> [a]
statesNFA (qs,_,_,_,_,_) = qs

symbolsNFA :: NFA a b -> [b]
symbolsNFA (_,ss,_,_,_,_) = ss

silentSymbolNFA :: NFA a b -> b
silentSymbolNFA (_,_,s,_,_,_) = s

transitionsNFA :: NFA a b -> [Transition a b]
transitionsNFA (_,_,_,ts,_,_) = ts

startStatesNFA :: NFA a b -> [a]
startStatesNFA (_,_,_,_,q0s,_) = q0s

acceptStatesNFA :: NFA a b -> [a]
acceptStatesNFA (_,_,_,_,_,as) = as

---------------
-- Constants --
---------------

epsilon :: Char
epsilon = 'ε'

------------------------------
-- NFA Correctness Checking --
------------------------------
