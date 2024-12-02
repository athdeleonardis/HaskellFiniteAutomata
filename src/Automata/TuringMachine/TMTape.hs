module FiniteAutomata.Automata.TuringMachine.TMTape

( TMTape

)
where

-- |Turing Machine Tape. ((pr, v, po), b) represents a TM tape currently pointing to the value 'v', with tape prior to the value 'pr', tape post the value 'po', and blank symbol 'b'.
type TMTape a = (([a], a, [a]), a)

tmTape :: a -> [a] -> TMTape a
tmTape b [] = (([], b, []), b)
tmTape b (s:str) = (([], s, str), b)

tmTapeRight :: TMTape a -> TMTape a
tmTapeRight ((lefts, v, []), b) = ((lefts ++ [v], b, []), b)
tmTapeRight ((lefts, v, r:rights), b) = ((v:lefts, r, rights), b)

tmTapeLeft :: TMTape a -> TMTape a
tmTapeLeft (([], v, rights), b) = (([], b, v:rights), b)
tmTapeLeft ((l:lefts, v, rights), b) = (lefts, l, v:rights)
