module Swift where

import Data.Numbers.Primes

printFizzBuzzes :: Int -> IO ()
printFizzBuzzes n = putStr . unlines . take n $ decoratedFizzbuzzFibs

decoratedFizzbuzzFibs :: [String]
decoratedFizzbuzzFibs = fmap fizzbuzz fibs

-- | The spec is a little unclear here on what to do when more than
--   one thing applies. Obviously FizzBuzz should be printed when n is
--   divisible by 15, otherwise it would never get used: this means
--   that treating the spec as conditions to be tested in order does
--   not work.
--
--   As the relative priority of primality checking vs Buzz and Fizz is not
--   made clear, we'll arbitrarily consider BuzzFizz
--   (primality) higher priority than either Buzz or Fizz.

fizzbuzz :: (Integral a, Show a) => a -> [Char]
fizzbuzz k
  | isPrime k      = "BuzzFizz"
  | 15 `divides` k = "FizzBuzz"
  | 5  `divides` k = "Fizz"
  | 3  `divides` k = "Buzz"
  | otherwise      = show k

fibs :: Integral a => [a]
fibs = 0:1:zipWith (+) fibs (tail fibs)

-- Need a special case for zero: everything divides it, but n `mod` 0
-- crashes. Partial functions are the pits.
divides :: Integral a => a -> a -> Bool
divides k n = k==0 || n `mod` k == 0
