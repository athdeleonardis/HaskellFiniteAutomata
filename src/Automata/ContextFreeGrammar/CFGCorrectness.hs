module FiniteAutomata.Automata.ContextFreeGrammar.CFGCorrectness

( CFGCorrectness
, cfgCorrectness
)
where

import Data.List (intercalate)
import FiniteAutomata.Automata.ContextFreeGrammar.CFG

data CFGCorrectness a
  = Correct
  | OverlappingSymbols [a]
  | InvalidStartVariable a
  | InvalidVariables [a]
  | InvalidExpansions [[a]]
  | MultipleIssues [CFGCorrectness a]
  deriving Eq

instance (Show a) => Show (CFGCorrectness a) where
  show Correct = "Correct"
  show (OverlappingSymbols ss) = "Incorrect overlapping variables and terminals: " ++ show ss
  show (InvalidStartVariable v0) = "Incorrect start variable: " ++ show v0
  show (InvalidVariables vs) = "Incorrect rule variables: " ++ show vs
  show (InvalidExpansions es) = "Incorrect rule expansions: " ++ show es
  show (MultipleIssues is) = intercalate "\n- " ("Multiple issues:" : map show is)

cfgOverlappingSymbols :: Eq a => CFG a -> CFGCorrectness a
cfgOverlappingSymbols cfg
  | length overlap == 0  = Correct
  | otherwise            = OverlappingSymbols overlap
  where
    overlap = filter (\x -> elem x $ cfgTerminals cfg) $ cfgVariables cfg

cfgInvalidStartVariable :: Eq a => CFG a -> CFGCorrectness a
cfgInvalidStartVariable cfg
  | sv `elem` cfgVariables cfg  = Correct
  | otherwise                   = InvalidStartVariable sv
  where
    sv = cfgStartingVariable cfg

cfgInvalidVariables :: Eq a => CFG a -> CFGCorrectness a
cfgInvalidVariables cfg
  | length badVars == 0  = Correct
  | otherwise            = InvalidVariables badVars
  where
    vs = cfgVariables cfg
    isVariable v = v `elem` vs
    badVars = filter (not . isVariable) $ map fst $ cfgRules cfg

cfgInvalidExpansions :: Eq a => CFG a -> CFGCorrectness a
cfgInvalidExpansions cfg
  | length badExps == 0  = Correct
  | otherwise            = InvalidExpansions badExps
  where
    symbols = (cfgVariables cfg) ++ (cfgTerminals cfg)
    isSymbol s = s `elem` symbols
    isBad str = length (filter (not . isSymbol) str) /= 0
    badExps = filter isBad $ map snd $ cfgRules cfg

cfgCorrectness :: Eq a => CFG a -> CFGCorrectness a
cfgCorrectness cfg
  | numIssues == 0  = Correct
  | numIssues == 1  = head issues
  | otherwise       = MultipleIssues issues
  where
    issues = filter (/= Correct) [cfgOverlappingSymbols cfg, cfgInvalidStartVariable cfg, cfgInvalidVariables cfg, cfgInvalidExpansions cfg]
    numIssues = length issues
