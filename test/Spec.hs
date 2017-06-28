{-# LANGUAGE ScopedTypeVariables #-}
import Test.Hspec
import Data.Numbers.Primes(primes, isPrime)
import Swift(fizzbuzz, divides, fibs)
import Test.QuickCheck(property, (==>), Positive(..))
import Debug.Trace
import Control.Monad (forM, void)
import Data.List(tails)

main = hspec spec

spec :: Spec
spec =
  describe "fizzbuzz++" $ do
    it "does the right thing on primes" $ do
      void $ forM (take 1000 $ fmap fizzbuzz primes) (`shouldBe` "BuzzFizz")

    it "handles Buzzes" $ do
      property $ \(Positive x::Positive Integer) ->
        let a = 3*x in
          not (5 `divides` a || a `elem` [0, 3]) ==>
          fizzbuzz a `shouldBe` "Buzz"

    it "handles Fizzes" $ do
      property $ \(Positive x::Positive Integer) ->
        let a = 5*x in
          not (3 `divides` a || a `elem` [0, 5]) ==>
          fizzbuzz a `shouldBe` "Fizz"

    it "falls back appropriately" $
      property $ \(Positive x:: Positive Integer) -> not (3 `divides` x || 5 `divides` x || isPrime x) ==>
        fizzbuzz x `shouldBe` show x

    it "generates fibonaccis accurately" $
      -- despite appearances the lambda is total, as fibs is an endless list
      void $ forM (take 1000 $ tails fibs) $ \(a:b:c:_) -> a + b `shouldBe` c
