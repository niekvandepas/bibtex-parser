module Debug where
import Debug.Trace (trace)

dump :: String -> a -> a
dump s = trace ("\n***********************\n" ++ s ++ "\n***********************\n")

runLog :: (Show a) => a -> a
runLog x = dump (show x) x
