module CFG

( CFG
, CFGRule
, cfgVariables
, cfgTerminals
, cfgRules
, cfgStartingVariable
, cfgIterate
, cfgIterateN
, cfgStringIsTerminal
)
where

import ArraySplit

--
-- CFG.hs types
--

-- |A key value pair, from variable to a string of variables and terminals
type CFGRule a = (a, [a])

-- |A 'Context Free Grammer', (variables, terminals, rules, starting variable)
type CFG a = ([a], [a], [CFGRule a], a)

-- |Get the variables of a context free grammar
cfgVariables :: CFG a -> [a]
cfgVariables (vs,_,_,_) = vs

-- |Get the terminals of a context free grammar
cfgTerminals :: CFG a -> [a]
cfgTerminals (_,ts,_,_) = ts

-- |Get the rules of a context free grammar
cfgRules :: CFG a -> [CFGRule a]
cfgRules (_,_,rs,_) = rs

-- |Get the starting variable of a context free grammar
cfgStartingVariable :: CFG a -> a
cfgStartingVariable (_,_,_,sv) = sv

-- |Get all possible rules where the variable is the key in a particular context free grammar
rules :: Eq a => CFG a -> a -> [CFGRule a]
rules cfg v = filter ((==v) . fst) $ cfgRules cfg

-- |Get all possible results where the variable is the key in a particular context free grammar
allPosts :: Eq a => CFG a -> a -> [[a]]
allPosts cfg v = map snd $ rules cfg v

expand :: (a -> [[a]]) -> ArraySplit a -> [[a]]
expand expander (priors, v, posts) = map (\x -> priors ++ x ++ posts) $ expander v

conditionalExpandAll :: (a -> Bool) -> (a -> [[a]]) -> [a] -> [[a]]
conditionalExpandAll cnd expander str =
  let maybeStrSplit = tryArraySplit str
  in case maybeStrSplit of
      Nothing -> []
      Just strSplit -> concat $ conditionalExpandAllInternal cnd expander strSplit      

conditionalExpandAllInternal :: (a -> Bool) -> (a -> [[a]]) -> ArraySplit a -> [[[a]]]
conditionalExpandAllInternal cnd expander (priors, v, posts) =
  let expanded = if (cnd v) then expand expander (priors, v, posts) else [priors ++ [v] ++ posts]
      maybeNextSplit = tryArraySplitIterate (priors, v, posts)
  in case maybeNextSplit of
    Nothing -> [expanded]
    Just nextSplit -> expanded : (conditionalExpandAllInternal cnd expander nextSplit)

-- |Take an array of strings and iterate them using the rules of the context free grammar
cfgIterate :: Eq a => CFG a -> [[a]] -> [[a]]
cfgIterate cfg strs = concat $ map (conditionalExpandAll notTerminal (allPosts cfg)) strs
  where
    notTerminal v = length (filter (==v) $ cfgTerminals cfg) == 0

-- |Start from the context free grammar's starting variable, and iterate N times
cfgIterateN :: Eq a => CFG a -> Int -> [[a]]
cfgIterateN cfg n = cfgIterateNInternal cfg n [[cfgStartingVariable cfg]]

cfgIterateNInternal :: Eq a => CFG a -> Int -> [[a]] -> [[a]]
cfgIterateNInternal cfg n strs
  | n <= 0     = strs
  | otherwise  = cfgIterateNInternal cfg (n-1) $ cfgIterate cfg strs

-- | Returns true if the string is only made up of the context free grammar's terminals, and false otherwise
cfgStringIsTerminal :: Eq a => CFG a -> [a] -> Bool
cfgStringIsTerminal cfg str = all (\x -> x `elem` (cfgTerminals cfg)) str
