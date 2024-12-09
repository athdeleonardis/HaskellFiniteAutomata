module FiniteAutomata.Automata.PushdownAutomata.DPDA

( DPDAState
, dpdaInitialState
, dpdaStep
, dpdaStepEmptyString
, dpdaSimulate
, dpdaStateIsAccept
, dpdaCheckString
)
where

import Data.List (find)
import FiniteAutomata.Automata.PushdownAutomata.PDA

type DPDAState a b = Maybe (PDAState a b)

dpdaInitialState :: PDA a b -> DPDAState a b
dpdaInitialState pda = Just (pdaStartState pda, [pdaStartStackSymbol pda])

dpdaRuleKeyToValue :: (Eq a, Eq b) => PDA a b -> PDARuleKey a b -> Maybe (PDARuleValue a b)
dpdaRuleKeyToValue pda key = fmap snd $ find ((==key) . fst) $ pdaRules pda

dpdaStep :: (Eq a, Eq b) => PDA a b -> b -> DPDAState a b -> DPDAState a b
dpdaStep _ _ Nothing = Nothing
dpdaStep pda _ (Just (_, [])) = Nothing
dpdaStep pda i (Just (q, s:ss)) = pdaApplyRuleValue (q, s:ss) <$> dpdaRuleKeyToValue pda (q, i, s)

dpdaStepEmptyString :: (Eq a, Eq b) => PDA a b -> DPDAState a b -> DPDAState a b
dpdaStepEmptyString _ Nothing = Nothing
dpdaStepEmptyString _ (Just (q, [])) = Just (q, [])
dpdaStepEmptyString pda (Just (q, stack)) =
  let maybeRuleValue = dpdaRuleKeyToValue pda (q, pdaInputEmptySymbol pda, head stack)
  in case maybeRuleValue of
    Nothing -> Just (q, stack)
    Just ruleValue -> dpdaStepEmptyString pda $ Just $ pdaApplyRuleValue (q, stack) ruleValue

dpdaSimulate :: (Eq a, Eq b) => PDA a b -> [b] -> DPDAState a b
dpdaSimulate pda str = dpdaStepEmptyString pda $ snd $ until ended doStep (str, dpdaInitialState pda)
  where
    ended (_, Nothing) = True
    ended ([], _) = True
    ended _ = False
    doStep (i:is, state) = (is, dpdaStep pda i state)

dpdaStateIsAccept :: (Eq a) => PDAAccept a -> DPDAState a b -> Bool
dpdaStateIsAccept _ Nothing = False
dpdaStateIsAccept EmptyStack (Just (q, stack)) = null stack
dpdaStateIsAccept (AcceptStates qs) (Just (q, _)) = q `elem` qs

dpdaCheckString :: (Eq a, Eq b) => PDA a b -> [b] -> Bool
dpdaCheckString pda str = dpdaStateIsAccept (pdaAcceptanceCriteria pda) $ dpdaSimulate pda str
