module FiniteAutomata.Util.ArraySplit

( ArraySplit
, tryArraySplitIterate
, tryArraySplit
)
where

type ArraySplit a = ([a], a, [a])

tryArraySplitIterate :: ArraySplit a -> Maybe (ArraySplit a)
tryArraySplitIterate (priors, v, posts)
  | length posts == 0  = Nothing
  | otherwise          = Just (priors ++ [v], head posts, tail posts)

tryArraySplit :: [a] -> Maybe (ArraySplit a)
tryArraySplit arr
  | length arr == 0  = Nothing
  | otherwise        = Just ([], head arr, tail arr)
