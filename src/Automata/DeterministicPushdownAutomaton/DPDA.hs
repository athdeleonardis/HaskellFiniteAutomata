module FiniteAutomata.Automata.DeterministicPushdownAutomaton.DPDA

( DPDA
, DPDAAccept (..)
, dpdaStates
, dpdaStackSymbols
, dpdaInputSymbols
, dpdaInputEmptySymbol
, dpdaRules
, dpdaStartState
, dpdaStartStackSymbol
, dpdaAcceptanceCriteria
, dpdaInitialState
, dpdaStep
, dpdaStepEmptyString
, dpdaSimulate
, dpdaStateIsAccept
, dpdaCheckString
)
where

import Data.List (find)

-- |DPDARuleKey. (q, a, A) represents a DPDARuleKey with prior state 'q', input symbol 'a' and stack symbol to pop 'A'.
type DPDARuleKey a b = (a, b, b)
-- |DPDARuleValue. (q, A[]) represents a DPDARuleValue with post state 'q', and stack symbols to push 'A[]'.
type DPDARuleValue a b = (a, [b])
type DPDARule a b = (DPDARuleKey a b, DPDARuleValue a b)

-- |DPDAState. (q, s[]) represents a DPDAState with current state 'q' and stack 's[]'.
type DPDAState a b = (a, [b])

data DPDAAccept a = EmptyStack | AcceptStates [a]
  deriving Show

-- |Deterministic Pushdown Automata. (qs,ss,is,ε,rs,q0,s0,as) represent a DPDA with states 'qs', stack symbols 'ss', input symbols 'is', input empty symbol 'ε', rules 'rs', initial state 'q0', initial stack symbol 's0', and accept states 'as'.
type DPDA a b = ([a], [b], [b], b, [DPDARule a b], a, b, DPDAAccept a)

dpdaStates :: DPDA a b -> [a]
dpdaStates (qs,_,_,_,_,_,_,_) = qs

dpdaStackSymbols :: DPDA a b -> [b]
dpdaStackSymbols (_,ss,_,_,_,_,_,_) = ss

dpdaInputSymbols :: DPDA a b -> [b]
dpdaInputSymbols (_,_,is,_,_,_,_,_) = is

dpdaInputEmptySymbol :: DPDA a b -> b
dpdaInputEmptySymbol (_,_,_,ie,_,_,_,_) = ie

dpdaRules :: DPDA a b -> [DPDARule a b]
dpdaRules (_,_,_,_,rs,_,_,_) = rs

dpdaStartState :: DPDA a b -> a
dpdaStartState (_,_,_,_,_,q0,_,_) = q0

dpdaStartStackSymbol :: DPDA a b -> b
dpdaStartStackSymbol (_,_,_,_,_,_,s0,_) = s0

dpdaAcceptanceCriteria :: DPDA a b -> DPDAAccept a
dpdaAcceptanceCriteria (_,_,_,_,_,_,_,a) = a

dpdaRuleKeyToValue :: (Eq a, Eq b) => DPDA a b -> DPDARuleKey a b -> Maybe (DPDARuleValue a b)
dpdaRuleKeyToValue dpda key = fmap snd $ find ((==key) . fst) $ dpdaRules dpda

dpdaApplyRuleValue :: DPDAState a b -> Maybe (DPDARuleValue a b) -> Maybe (DPDAState a b)
dpdaApplyRuleValue _ Nothing = Nothing
dpdaApplyRuleValue (_, s:ss) (Just (qn, ssn)) = Just (qn, ssn ++ ss)

dpdaInitialState :: DPDA a b -> DPDAState a b
dpdaInitialState dpda = (dpdaStartState dpda, [dpdaStartStackSymbol dpda])

dpdaStep :: (Eq a, Eq b) => DPDA a b -> b -> Maybe (DPDAState a b) -> Maybe (DPDAState a b)
dpdaStep _ _ Nothing = Nothing
dpdaStep dpda _ (Just (_, [])) = Nothing
dpdaStep dpda i (Just (q, s:ss)) = dpdaApplyRuleValue (q, s:ss) $ dpdaRuleKeyToValue dpda (q, i, s)

dpdaStepEmptyString :: (Eq a, Eq b) => DPDA a b -> Maybe (DPDAState a b) -> Maybe (DPDAState a b)
dpdaStepEmptyString _ Nothing = Nothing
dpdaStepEmptyString _ (Just (q, [])) = Just (q, [])
dpdaStepEmptyString dpda (Just (q, stack)) =
  let maybeRuleValue = dpdaRuleKeyToValue dpda (q, dpdaInputEmptySymbol dpda, head stack)
  in case maybeRuleValue of
    Nothing -> Just (q, stack)
    Just ruleValue -> dpdaStepEmptyString dpda $ dpdaApplyRuleValue (q, stack) maybeRuleValue

dpdaSimulate :: (Eq a, Eq b) => DPDA a b -> [b] -> Maybe (DPDAState a b)
dpdaSimulate dpda str = dpdaStepEmptyString dpda $ snd $ until ended doStep $ (str, Just $ dpdaInitialState dpda)
  where
    ended (_, Nothing) = True
    ended ([], _) = True
    ended _ = False
    doStep (i:is, state) = (is, dpdaStep dpda i state)

dpdaStateIsAccept :: (Eq a) => DPDAAccept a -> Maybe (DPDAState a b) -> Bool
dpdaStateIsAccept _ Nothing = False
dpdaStateIsAccept EmptyStack (Just (q, stack)) = null stack
dpdaStateIsAccept (AcceptStates qs) (Just (q, _)) = q `elem` qs

dpdaCheckString :: (Eq a, Eq b) => DPDA a b -> [b] -> Bool
dpdaCheckString dpda str = dpdaStateIsAccept (dpdaAcceptanceCriteria dpda) $ dpdaSimulate dpda str
