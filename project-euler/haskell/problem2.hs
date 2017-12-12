{-
Each new term in the Fibonacci sequence is generated by adding the previous two terms. By starting with 1 and 2, the first 10 terms will be:

1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...

By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms.
-}

{-
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)
-}

{-
fib :: Int -> Int
fib = fst . fib2
 
-- | Return (fib n, fib (n + 1))
fib2 :: Int -> (Int, Int)
fib2 0 = (1, 1)
fib2 1 = (1, 2)
fib2 n
 | even n    = (a*a + b*b, c*c - a*a)
 | otherwise = (c*c - a*a, b*b + c*c)
 where (a,b) = fib2 (n `div` 2 - 1)
       c     = a + b
-}

fib n = round $ phi ** fromIntegral n / sq5
  where
    sq5 = sqrt 5 :: Double
    phi = (1 + sq5) / 2

fibs :: Int -> [Int]
fibs 0 = []
fibs 1 = [1]
fibs 2 = [1,2]
fibs n = if n < 0 then [] else fib <$> [2..n+1]

main :: IO ()
main = do
  print $ filter even $ fibs 1000
--  print $ filter even $ fibs 10
--  print $ filter even $ fibs 4000000
