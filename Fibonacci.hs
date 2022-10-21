fibonacci :: Int -> Int
fibonacci = (!!) fib
  where
    fib = 0 : 1 : zipWith (+) fib (tail fib)