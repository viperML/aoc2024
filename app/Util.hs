module Util where
import Debug.Trace (traceShow)

traceVal :: (Show a) => a -> a
traceVal x = traceShow x x
