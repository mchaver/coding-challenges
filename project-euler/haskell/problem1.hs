{-
If we list all the natural numbers below 10 that are multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000.
-}

solutionFold :: Int -> Int
solutionFold range =
  foldl (\ys x -> if mod x 3 == 0 || mod x 5 == 0 then x + ys else ys) 0 [0..range-1]

solutionFilter :: Int -> Int
solutionFilter range =
  sum $ filter (\x -> mod x 3 == 0 || mod x 5 == 0) [0..range-1]

main :: IO ()
main = do
  print $ solutionFold $ 10
  print $ solutionFilter $ 10
  print $ solutionFold $ 1000
  print $ solutionFilter $ 1000
