import Data.Maybe

root :: Fractional a
  => Int -- ˆ Maximum number of iterations.
  -> (a -> Bool) -- ˆ An acceptable error (precision).
  -> (a -> a) -- ˆ Function f to find zero for.
  -> (a -> a) -- ˆ Derivative f' of function f.
  -> a -- ˆ Initial guess.
  -> Maybe (Int, a) -- ˆ Number of iterations and root
  -- (where function is zero), if found.
root maxIterations acceptableError f df initialGuess = ret
  where 
    lst = iterate (\x -> x - (f x)/(df x)) initialGuess
    indecies = [0..]
    mergedList = zipWith (\x y -> (x, y)) indecies lst
    check (x, y) = (not (acceptableError (abs (f y)))) && x <= maxIterations 
    result = takeWhile check mergedList
    ret = if length result == maxIterations + 1 then Nothing 
          else listToMaybe (drop (length result) mergedList)
    
main :: IO ()
main = do
  let x = root 100 (< 1e-7) (\x -> x*x - 2) (\x -> 2*x) 123
  print (x)
  let y = root 100 (< 1e-12) cos (negate . sin) 1.0
  print (y)
  let z = root 100 (< 1e-12) cos (negate . sin) 0
  print (z)